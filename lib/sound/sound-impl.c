#include <stdio.h>
#include <string.h>
#include <portaudio.h>
#include <opusfile.h>

typedef enum {
    PAUSED,
    STOPPING,
    PLAYING
} state_t;

typedef struct {
    OggOpusFile* stream;
    float volume;
    state_t state;
} channel_t;

#define CHANNELS_NUMBER 16

static channel_t channels[CHANNELS_NUMBER] = {0};
static PaStream* paStream;

static int
read_channel(channel_t chan, float* dest, unsigned long frameCount) {
    int ret = 1;
    while (frameCount > 0) {
        ret = op_read_float_stereo(chan.stream, dest, frameCount*2);
        frameCount -= ret;
        dest += ret * 2;
        if (ret == 0) {
            op_pcm_seek(chan.stream, 0);
        }
    }
    return ret;
}

static int
mix(float* destination, float* source, float volume, unsigned long length) {
    for (unsigned long i = 0; i < length; i++) {
        destination[i] = destination[i] + (source[i] * volume);
    }
    return 0;
}

static int
myCallback(const void* input, void* output, unsigned long frameCount, const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags, void* userData) {
    (void) input;
    float temp_buf[frameCount*2];
    int ret = 0;
    float* out = (float*) output;
    channel_t* channels = (channel_t*) userData;

    memset(output, 0, frameCount*2*sizeof(float));

    for (int i = 0; i < CHANNELS_NUMBER; i++) {
        channel_t chan = channels[i];
        if (chan.state == PLAYING) {
            ret = read_channel(chan, temp_buf, frameCount);
            mix(out, temp_buf, chan.volume, frameCount * 2);
        }
        if (chan.state == STOPPING) {
            op_pcm_seek(chan.stream, 0);
            chan.state = PAUSED;
        }
    }

    return paContinue;
}

static void
enumerate() {
    const PaDeviceInfo* deviceInfo;
    const PaHostApiInfo* apiInfo;
    int numDevices = Pa_GetDeviceCount();
    for (int i = 0; i < numDevices; i++) {
        deviceInfo = Pa_GetDeviceInfo(i);
        apiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);
        printf("%d: %s - %s\n", i, apiInfo->name,deviceInfo->name);
    }
}

static int
startup() {
    Pa_Initialize();
    // enumerate();

    Pa_OpenDefaultStream(&paStream, 0, 2, paFloat32, 48000, paFramesPerBufferUnspecified, myCallback, channels);

    Pa_StartStream(paStream);
}

static int
shutdown() {
    Pa_StopStream(paStream);
    Pa_CloseStream(paStream);
    Pa_Terminate();
    return 0;
}
