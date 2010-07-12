#include <alsa/asoundlib.h>

void set_instrument(snd_seq_t ***seq, snd_seq_addr_t **dest_port,
    const int *queue, 
    const int tick, const int channel, const int instrument) {
  snd_seq_event_t ev;
  ev.queue = *queue;
  ev.source.port = 0;
  ev.type = SND_SEQ_EVENT_PGMCHANGE;
  ev.time.tick = tick;
  ev.data.control.channel = channel;
  ev.data.control.value = instrument;
  ev.dest = **dest_port;
  snd_seq_event_output(**seq, &ev);
  printf("set_instrument: %d %d %d\n", tick, channel, instrument);
}

static void note_event(snd_seq_t ***seq, snd_seq_addr_t **dest_port, 
    const int *queue, const int tick, const int type, const int channel, 
    const int note, int velocity) {
  snd_seq_event_t ev;
  ev.queue = *queue;
  ev.source.port = 0;
  ev.flags = SND_SEQ_TIME_STAMP_TICK;
  ev.dest = **dest_port;
  ev.type = type;
  ev.time.tick = tick;
  ev.data.note.channel = channel;
  ev.data.note.note = note;
  ev.data.note.velocity = velocity;
  snd_seq_event_output(**seq, &ev);
  printf("note_event: %d %d %d %d %d\n", tick, type, channel, note, velocity);
}

void note_on(snd_seq_t ***seq, snd_seq_addr_t **dest_port, const int *queue,
    const int tick, const int channel, const int note, const int velocity) {
  note_event(seq, dest_port, queue, tick, SND_SEQ_EVENT_NOTEON,
    channel, note, velocity);
}

void note_off(snd_seq_t ***seq, snd_seq_addr_t **dest_port, const int *queue,
    const int tick, const int channel, const int note) {
  note_event(seq, dest_port, queue, tick, SND_SEQ_EVENT_NOTEOFF, 
    channel, note, 0);
}

void midi_initialize(snd_seq_t ***seq, snd_seq_addr_t **dest_port, 
    int *queue, const char *client_name, const char *address_str) {
  *seq = malloc(sizeof(snd_seq_t *));
  *dest_port = malloc(sizeof(snd_seq_addr_t));
  snd_seq_open(*seq, "default", SND_SEQ_OPEN_DUPLEX, 0);
  snd_seq_set_client_name(**seq, client_name);
  snd_seq_parse_address(**seq, *dest_port, address_str);
  snd_seq_port_info_t *pinfo;
  snd_seq_port_info_alloca(&pinfo);
  snd_seq_port_info_set_name(pinfo, client_name);
  snd_seq_port_info_set_type(pinfo,
    SND_SEQ_PORT_TYPE_MIDI_GENERIC | SND_SEQ_PORT_TYPE_APPLICATION);
  snd_seq_create_port(**seq, pinfo);
  *queue = snd_seq_alloc_named_queue(**seq, client_name);
  snd_seq_connect_to(**seq, 0, (*dest_port)->client, (*dest_port)->port);
  snd_seq_start_queue(**seq, *queue, NULL);
}

void midi_finalize(snd_seq_t ***seq, snd_seq_addr_t **dest_port) {
  snd_seq_drain_output(**seq);
  snd_seq_sync_output_queue(**seq);
  snd_seq_close(**seq);
  free(*dest_port);
  free(*seq);
}

