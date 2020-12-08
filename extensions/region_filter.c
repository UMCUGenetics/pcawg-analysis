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

  free (file_name); file_name = NULL;

  return (scm_values
          (scm_list_2
           (SCM_BOOL_F, scm_from_latin1_string (error_msg))));
}

SCM
print_bam_index_file_error (char *file_name)
{
  /* Now this looks quite Lispy, eh? */
  char error_msg[255];
  snprintf (error_msg, 255, "Cannot open index for '%s'.", file_name);

  free (file_name); file_name = NULL;

  return (scm_values
          (scm_list_2
           (SCM_BOOL_F, scm_from_latin1_string (error_msg))));
}

SCM
print_bam_header_error ()
{
  return (scm_values
          (scm_list_2
           (SCM_BOOL_F, scm_from_latin1_string ("Cannot read header."))));
}

SCM
extract_reads_for_region (SCM input_scm,
                          SCM output_scm,
                          SCM output_format_scm,
                          SCM region_scm)
{
  /* Prepare the buffers needed to read the BAM file.
   * -------------------------------------------------------------------- */

  bam_hdr_t *bam_header        = NULL;
  htsFile   *bam_input_stream  = NULL;
  htsFile   *bam_output_stream = NULL;
  hts_idx_t *bam_index         = NULL;
  hts_itr_t *iterator          = NULL;
  char      *input_file        = NULL;
  char      *output_file       = NULL;
  char      *output_format     = NULL;
  char      *region            = NULL;
  SCM       output             = NULL;
  int       state              = 0;

  input_file = scm_to_locale_string (input_scm);
  bam_input_stream = hts_open (input_file, "r");
  if (! bam_input_stream)
    return print_bam_file_error (input_file);

  bam_index = sam_index_load (bam_input_stream, input_file);
  if (! bam_index)
    return print_bam_index_file_error (input_file);

  output_format = scm_to_locale_string (output_format_scm);
  output_file   = scm_to_locale_string (output_scm);

  bam_output_stream = (! strcmp (output_format, "bam"))
    ? hts_open (output_file, "wb")
    : hts_open (output_file, "w");

  free (output_format); output_format = NULL;

  if (! bam_output_stream)
    {
      free (input_file);             input_file = NULL;
      hts_close (bam_input_stream);  bam_input_stream = NULL;
      hts_idx_destroy (bam_index);   bam_index = NULL;

      return print_bam_file_error (output_file);
    }

  /* Read/write the SAM/BAM/CRAM header.
   * -------------------------------------------------------------------- */
  bam_header = sam_hdr_read (bam_input_stream);
  if ((! bam_header) || (sam_hdr_write (bam_output_stream, bam_header) < 0))
    {
      free (input_file);             input_file = NULL;
      hts_close (bam_input_stream);  bam_input_stream = NULL;
      hts_idx_destroy (bam_index);   bam_index = NULL;

      hts_close (bam_input_stream);  bam_input_stream = NULL;
      hts_close (bam_output_stream); bam_output_stream = NULL;

      return print_bam_header_error ();
    }

  /* Read -> filter -> write the SAM/BAM/CRAM reads.
   * ----------------------------------------------------------------------- */

  region = scm_to_locale_string (region_scm);

  iterator = sam_itr_querys (bam_index, bam_header, region);
  if (iterator != 0)
    {
      bam1_t *alignment = bam_init1 ();
      while ((state = sam_itr_next (bam_input_stream,
                                    iterator,
                                    alignment)) >= 0)
        {
          if (sam_write1 (bam_output_stream, bam_header, alignment) <= 0)
            {
              output = scm_values
                (scm_list_2
                 (SCM_BOOL_F, scm_from_latin1_string
                  ("An error occurred while processing an alignment record.")));

              break;
            }
    	}

      bam_destroy1 (alignment); alignment = NULL;
    }
  else
    output = scm_values
              (scm_list_2
               (SCM_BOOL_F, scm_from_latin1_string
                ("Cannot find region.")));

  sam_itr_destroy (iterator);    iterator = NULL;
  hts_idx_destroy (bam_index);   bam_index = NULL;

  free (input_file);             input_file = NULL;
  free (output_file);            output_file = NULL;
  free (region);                 region = NULL;

  bam_hdr_destroy (bam_header);  bam_header = NULL;
  hts_close (bam_input_stream);  bam_input_stream = NULL;
  hts_close (bam_output_stream); bam_output_stream = NULL;

  if (output == NULL)
    {
      if (state == -1)
        output = scm_values
          (scm_list_2
           (SCM_BOOL_T, SCM_UNDEFINED));
      else
        scm_values
          (scm_list_2
           (SCM_BOOL_F, scm_from_latin1_string
            ("Processing reads ended prematurely.")));
    }

  return output;
}

void
init_region_filter ()
{
  hts_verbose = 0;
  scm_c_define_gsubr ("extract-reads-for-region", 4, 0, 0, extract_reads_for_region);
}
