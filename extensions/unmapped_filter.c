/*
 * Copyright (C) 2020  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <htslib/sam.h>
#include <libguile.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

SCM
print_bam_file_error (const char *file_name)
{
  /* Now this looks quite Lispy, eh? */
  char error_msg[255];
  snprintf (error_msg, 255, "Cannot open '%s'.", file_name);

  return (scm_values
          (scm_list_2
           (SCM_BOOL_F, scm_from_latin1_string (error_msg))));
}

SCM
print_bam_header_error (const char *file_name)
{
  return (scm_values
          (scm_list_2
           (SCM_BOOL_F, scm_from_latin1_string ("Cannot read header."))));
}

SCM
extract_unmapped_reads (SCM input_scm,
                        SCM output_scm,
                        SCM output_format_scm,
                        SCM softclip_min_length_scm,
                        SCM exclude_softclips)
{
  /* Scheme to C conversions.
   * -------------------------------------------------------------------- */
  char *input_file        = scm_to_locale_string (input_scm);
  char *output_file       = scm_to_locale_string (output_scm);
  char *output_format     = scm_to_locale_string (output_format_scm);
  int softclip_min_length = scm_to_int (softclip_min_length_scm);

  /* Prepare the buffers needed to read the BAM file.
   * -------------------------------------------------------------------- */
  bam_hdr_t *bam_header = NULL;
  htsFile   *bam_input_stream = NULL;
  htsFile   *bam_output_stream = NULL;

  bam_input_stream = hts_open (input_file, "r");
  if (!bam_input_stream)
    return print_bam_file_error (input_file);

  bam_output_stream = (! strcmp (output_format, "bam"))
    ? hts_open (output_file, "wb")
    : hts_open (output_file, "w");

  if (!bam_output_stream)
    {
      hts_close (bam_input_stream);
      return print_bam_file_error (output_file);
    }

  /* Read/write the SAM/BAM/CRAM header.
   * -------------------------------------------------------------------- */
  bam_header = sam_hdr_read (bam_input_stream);
  if (!bam_header)
    {
      hts_close (bam_input_stream);
      return print_bam_header_error (input_file);
    }

  int header_write_result = sam_hdr_write (bam_output_stream, bam_header);
  if (header_write_result < 0)
    {
      hts_close (bam_input_stream);
      hts_close (bam_output_stream);
      return print_bam_header_error (output_file);
    }

  /* Use the index to obtain the total number of unmapped reads. */
  hts_idx_t *bam_index = NULL;
  uint64_t total_unmapped_reads    = 0;
  uint64_t observed_unmapped_reads = 0;

  bam_index = sam_index_load (bam_input_stream, input_file);
  total_unmapped_reads = hts_idx_get_n_no_coor (bam_index);
  hts_idx_destroy (bam_index);

  /* Read -> filter ->write the SAM/BAM/CRAM reads.
   * ----------------------------------------------------------------------- */

  bam1_t *alignment = bam_init1();
  while (sam_read1 (bam_input_stream, bam_header, alignment) > 0)
    {
      bool is_soft_clipped = false;
      bool is_unmapped     = false;

      /* Look for soft-clipped reads.
       * ------------------------------------------------------------------- */

      /* We must have at least one "mapped" operation, and one "soft clip"
       * operation; so at least two operations for soft-clipping to be
       * possible. */
      if (scm_is_false (exclude_softclips) && alignment->core.n_cigar > 1)
        {
          uint32_t *cigar           = bam_get_cigar (alignment);
          int32_t  iterator         = 0;
          uint32_t operator         = 0;
          int32_t  operation_length = 0;
          for (; iterator < alignment->core.n_cigar; iterator++)
            {
              operator         = bam_cigar_op (cigar[iterator]);
              operation_length = bam_cigar_oplen (cigar[iterator]);

              if (operator == BAM_CSOFT_CLIP &&
                  operation_length > softclip_min_length)
                is_soft_clipped = true;
            }
        }

      /* Look in the flag field.
       * ------------------------------------------------------------------- */
      #define flag alignment->core.flag
      is_unmapped = (flag & BAM_FUNMAP)
        || ((flag & BAM_FPAIRED) && (flag & BAM_FUNMAP)  && (flag & BAM_FMUNMAP))
        || ((flag & BAM_FPAIRED) && (flag & BAM_FMUNMAP) && (flag & BAM_FREAD1))
        || ((flag & BAM_FPAIRED) && (flag & BAM_FUNMAP)  && (flag & BAM_FREAD2));

      if (is_unmapped) observed_unmapped_reads++;
      if (is_soft_clipped || is_unmapped)
        {
          if (sam_write1 (bam_output_stream, bam_header, alignment) <= 0)
            return
              (scm_values
               (scm_list_2
                (SCM_BOOL_F, scm_from_latin1_string
                 ("An error occurred while writing an alignment record."))));
        }
    }

  free (input_file);
  free (output_file);
  free (output_format);

  bam_destroy1 (alignment);
  bam_hdr_destroy (bam_header);
  hts_close (bam_input_stream);
  hts_close (bam_output_stream);

  return (scm_values
          (scm_list_2
           (SCM_BOOL_T,
            (scm_list_2
             (scm_from_uint64 (total_unmapped_reads),
              scm_from_uint64 (observed_unmapped_reads))))));
}

void
init_unmapped_filter ()
{
  hts_verbose = 0;
  scm_c_define_gsubr ("extract-unmapped-reads", 5, 0, 0, extract_unmapped_reads);
}
