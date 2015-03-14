#include <stdio.h>
#include "portaudio.h"

PaStream *stream;

int start_lol(void);
int pa_write(float *buffer, int frames_per_buffer);
int mid_lols(void);
int end_lol(void);
