#include "my_f.h"

/*
PaError my_init(PaStreamParameters *output_params_p, PaStream **stream_p) {
    PaError err;
    int sample_rate = 44100;
    int frames_per_buffer = 600;

    output_params_p->device = Pa_GetDefaultOutputDevice();
    if (output_params_p->device == paNoDevice) {
      fprintf(stderr, "Error: No default output device.\n");
      return err;
    }

    output_params_p->channelCount = 2;
    output_params_p->sampleFormat = paFloat32 | paNonInterleaved;
    output_params_p->suggestedLatency =
        Pa_GetDeviceInfo(output_params_p->device)->defaultLowOutputLatency;
    output_params_p->hostApiSpecificStreamInfo = NULL;
    err = Pa_OpenStream(
        stream_p,
        NULL, // no input
        output_params_p,
        sample_rate,
        frames_per_buffer,
        paClipOff, // we won't output out of range so don't bother clipping
        NULL, // last two null for blocking IO
        NULL);
    if (err != paNoError) return err;

    err = Pa_StartStream(*stream_p);
    return err;
}
*/

int my_f(int x) {
    return x + 1;
}

int my_all() {
    PaStreamParameters output_params;
    PaStream *stream_p;
    PaError err;
    int frames_per_buffer = 1024;
    int sample_rate = 44100;
    int table_size = 200;
    float pi = 3.14159265;
    float left_buff[frames_per_buffer];
    void *buffers[2];
    float sine[table_size];
    int left_phase = 0;
    int left_inc = 2;
    int i, j, k;
    int buffer_count;
    int num_secs = 1;

    for (i = 0; i < table_size; i++) {
        sine[i] = (float)sin(((double)i/(double)table_size) * pi * 2.0);
    }
 
    err = Pa_Initialize();
    if (err != paNoError) goto error;

    output_params.device = Pa_GetDefaultOutputDevice();
    if (output_params.device == paNoDevice) {
        fprintf(stderr, "Error: No default output device.\n");
        err = -1;
        goto error;
    }

    output_params.channelCount = 2;
    output_params.sampleFormat = paFloat32 | paNonInterleaved;
    output_params.suggestedLatency =
        Pa_GetDeviceInfo(output_params.device)->defaultLowOutputLatency;
    output_params.hostApiSpecificStreamInfo = NULL;
    err = Pa_OpenStream(
        &stream_p,
        NULL, // no input
        &output_params,
        sample_rate,
        frames_per_buffer,
        paClipOff, // we won't output out of range so don't bother clipping
        NULL, // last two null for blocking IO
        NULL);
    if (err != paNoError) return err;

    err = Pa_StartStream(stream_p);
    if (err != paNoError) goto error;

    buffers[0] = left_buff;
    buffers[1] = left_buff;
    
    for (k = 0; k < 1; k++) {

        buffer_count = ((num_secs * sample_rate) / frames_per_buffer);

        for (i = 0; i < buffer_count; i++) {
            for (j = 0; j < frames_per_buffer; j++) {
                left_buff[j] = sine[left_phase];
                left_phase += left_inc;
                if (left_phase >= table_size) left_phase -= table_size;
            }
            err = Pa_WriteStream(stream_p, buffers, frames_per_buffer);
            if (err != paNoError) goto error;
        }   

        left_inc++;
    }
    err = Pa_StopStream(stream_p);
    if (err != paNoError) goto error;

    err = Pa_CloseStream(stream_p);
    if (err != paNoError) goto error;

    Pa_Terminate();
    return err;
error:
    Pa_Terminate();
    fprintf(stderr, "An error occured while using the portaudio stream\n");
    fprintf(stderr, "Error number: %d\n", err);
    fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
    return err;
}
