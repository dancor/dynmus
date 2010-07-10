#include "alsa_midi.c"

int main() {
  snd_seq_t **s7r = NULL;
  snd_seq_addr_t *dest_port = NULL;
  int queue;
  midi_initialize(&s7r, &dest_port, &queue, "dynmus", "128:0");
  set_instrument(&s7r, &dest_port, &queue, 0, 1, 41);
  note_on(&s7r, &dest_port, &queue, 0, 0, 60, 100);
  note_on(&s7r, &dest_port, &queue, 200, 1, 63, 40);
  note_off(&s7r, &dest_port, &queue, 400, 0, 60);
  note_off(&s7r, &dest_port, &queue, 600, 1, 63);
  midi_finalize(&s7r, &dest_port);
  return 0;
}
