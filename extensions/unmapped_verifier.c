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
print_bam_file_error (char *file_name)
{
  /* Now this looks quite Lispy, eh? */
  char error_msg[255];
  snprintf (error_msg, 255, "Cannot open '%s'.", file_name);

  free (file_name);

  return (scm_values
          (scm_list_3
           (SCM_BOOL_F,
            scm_from_latin1_string (error_msg),
            SCM_UNDEFINED)));
}

SCM
print_bam_header_error ()
{
  return (scm_values
          (scm_list_3
           (SCM_BOOL_F,
            scm_from_latin1_string ("Cannot read header."),
            SCM_UNDEFINED)));
}

SCM
print_bam_index_error ()
{
  return (scm_values
          (scm_list_3
           (SCM_BOOL_F,
            scm_from_latin1_string ("Cannot read index."),
            SCM_UNDEFINED)));
}

SCM
verify_unmapped_reads (SCM input_scm, SCM output_scm)
{
  /* Scheme to C conversions.
   * -------------------------------------------------------------------- */
  char *input_file        = scm_to_locale_string (input_scm);
  char *output_file       = scm_to_locale_string (output_scm);

  /* Prepare the buffers needed to read the BAM file.
   * -------------------------------------------------------------------- */
  bam_hdr_t *bam_input_header = NULL;
  bam_hdr_t *bam_output_header = NULL;
  htsFile   *bam_input_stream = NULL;
  htsFile   *bam_output_stream = NULL;

  bam_input_stream = hts_open (input_file, "r");
  if (! bam_input_stream)
    {
      free (output_file);
      return print_bam_file_error (input_file);
    }

  bam_output_stream = hts_open (output_file, "r");
  if (! bam_output_stream)
    {
      free (input_file);
      hts_close (bam_input_stream);
      return print_bam_file_error (output_file);
    }


  /* Read/write the SAM/BAM/CRAM header.
   * -------------------------------------------------------------------- */
  bam_input_header = sam_hdr_read (bam_input_stream);
  if (! bam_input_header)
    {
      free (input_file);
      free (output_file);
      hts_close (bam_input_stream);
      hts_close (bam_output_stream);
      return print_bam_header_error ();
    }

  bam_output_header = sam_hdr_read (bam_output_stream);
  if (! bam_output_header)
    {
      free (input_file);
      free (output_file);
      hts_close (bam_input_stream);
      hts_close (bam_output_stream);
      return print_bam_header_error ();
    }

  /* Use the index to obtain the total number of unmapped reads. */
  hts_idx_t *bam_input_index       = NULL;
  uint64_t input_unmapped_reads    = 0;
  uint64_t observed_unmapped_reads = 0;

  bam_input_index = sam_index_load (bam_input_stream, input_file);
  if (! bam_input_index)
    {
      free (input_file);
      free (output_file);
      hts_close (bam_input_stream);
      hts_close (bam_output_stream);
      return print_bam_index_error ();
    }

  input_unmapped_reads = hts_idx_get_n_no_coor (bam_input_index);
  hts_idx_destroy (bam_input_index);

  free (input_file);
  free (output_file);

  bam1_t *alignment = bam_init1();
  int state = 0;
  while ((state = sam_read1 (bam_output_stream, bam_output_header, alignment)) >= 0)
    {
      if ((alignment->core.flag & BAM_FUNMAP))
        observed_unmapped_reads++;
    }

  bam_destroy1 (alignment);
  bam_hdr_destroy (bam_input_header);
  bam_hdr_destroy (bam_output_header);
  hts_close (bam_input_stream);
  hts_close (bam_output_stream);

  return (state == -1)
    ? (scm_values
       (scm_list_3
        (SCM_BOOL_T,
         scm_from_uint64 (input_unmapped_reads),
         scm_from_uint64 (observed_unmapped_reads))))
    : (scm_values
          (scm_list_2
           (SCM_BOOL_F,
            scm_from_latin1_string ("Processing reads ended prematurely."))));
}

void
init_unmapped_verifier ()
{
  hts_verbose = 0;
  scm_c_define_gsubr ("verify-unmapped-reads", 2, 0, 0, verify_unmapped_reads);
}
