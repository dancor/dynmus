#include "lol.h"

int start_lol(void) {
    PaError err = 0;
    int sample_rate = 44100;
    int frames_per_buffer = 32;

    //printf("PortAudio Test: output sawtooth wave.\n");
    /* Initialize library before making any other calls. */
    err = Pa_Initialize();
    if (err) goto error;
    
    /* Open an audio I/O stream. */
    err = Pa_OpenDefaultStream(
        &stream,
        0,          /* no input channels */
        1,          /* mono output */
        paFloat32,  /* 32 bit floating point output */
        sample_rate,
        frames_per_buffer,
        NULL,
        NULL);
    if (err) goto error;

    err = Pa_StartStream(stream);
    if (err) goto error;
    return err;
error:
    Pa_Terminate();
    fprintf(stderr, "An error occured while using the portaudio stream\n");
    fprintf(stderr, "Error number: %d\n", err);
    fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
    return err;
}

int pa_write(float *buffer, int frames_per_buffer) {
    PaError err = 0;
    err = Pa_WriteStream(stream, buffer, frames_per_buffer);
    if (err) goto error;
    return err;
error:
    Pa_Terminate();
    fprintf(stderr, "An error occured while using the portaudio stream\n");
    fprintf(stderr, "Error number: %d\n", err);
    fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
    return err;
}

int mid_lols(void) {
    PaError err = 0;
    int sample_rate = 44100;
    int frames_per_buffer = 32;
    float cur_phase = 0.0;
    float buffer[32];
    unsigned int i;
    unsigned int j = 0;

    for (j = 0; j < 2 * sample_rate / frames_per_buffer; j++) {
        for (i = 0; i < frames_per_buffer; i++) {
            cur_phase += 0.01;
            if (cur_phase >= 0.5) {
                cur_phase -= 2.0f;
            }
            buffer[i] = cur_phase;
        }
        err = pa_write(buffer, frames_per_buffer);
        if (err) return err;
    }
    return err;
}

int end_lol(void) {
    PaError err = 0;

    err = Pa_StopStream(stream);
    if (err) goto error;
    err = Pa_CloseStream(stream);
    if (err) goto error;
    Pa_Terminate();
    //printf("Test finished.\n");
    return err;
error:
    Pa_Terminate();
    fprintf(stderr, "An error occured while using the portaudio stream\n");
    fprintf(stderr, "Error number: %d\n", err);
    fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
    return err;
}
