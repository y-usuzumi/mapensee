#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>
#include <jack/jack.h>
#include <jack/midiport.h>

jack_port_t *output_port;
jack_client_t *client;
jack_port_t *peer;
jack_nframes_t loop_index;
uint32_t char_idx;

jack_nframes_t sample_rate = 48000;

uint32_t c(int row, int col) {
  return row * 16 + col;
}

void write_note(void* port_buf, jack_nframes_t i, uint32_t* notes, size_t note_cnt, bool on) {
  unsigned char* buffer;
  for (int idx = 0; idx < note_cnt; idx++) {
    buffer = jack_midi_event_reserve(port_buf, i, 3);
    buffer[0] = on ? 0x90 : 0x80;  // on
    buffer[1] = notes[idx];     // note
    buffer[2] = 127;    // velocity
  }
}

void write_char(void* port_buf, jack_nframes_t i, uint32_t char_idx, bool on) {
  uint32_t* notes;
  size_t note_cnt;
  printf("%d\n", char_idx);
  switch (char_idx) {
  case 0:
    notes = (
             (uint32_t[])
      {
          c(0, 3), c(1, 3), c(2, 3), c(3, 3), c(4, 3), c(5, 3), c(6, 3), c(5, 2),
          c(2, 1), c(3, 0), c(2, 5), c(3, 6)
      });
    note_cnt = 12;
    break;
  case 1:
    notes = (
             (uint32_t[])
      {
          c(1, 0), c(1, 1), c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(1, 6),
            c(2, 0), c(2, 2), c(2, 4), c(2, 6),
            c(3, 0), c(3, 1), c(3, 2), c(3, 4), c(3, 5), c(3, 6),
          c(4, 0), c(4, 6),
          c(5, 0), c(5, 1), c(5, 2), c(5, 3), c(5, 4), c(5, 5), c(5, 6),
      });
    note_cnt = 26;
    break;
  case 2:
    notes = (
             (uint32_t[])
      {
          c(0, 2), c(0, 4), c(0, 6),
            c(1, 0), c(1, 1), c(1, 4),
            c(2, 1), c(2, 3), c(2, 4), c(2, 5), c(2, 6),
            c(3, 0), c(3, 1), c(3, 2), c(3, 4),
            c(4, 1), c(4, 5), c(4, 6),
            c(5, 0), c(5, 1), c(5, 2), c(5, 5),
            c(6, 1), c(6, 4), c(6, 6),
      });
    note_cnt = 25;
    break;
  case 3:
    notes = (
             (uint32_t[])
      {
          c(0, 1), c(0, 2), c(0, 3), c(0, 4), c(0, 5),
            c(1, 2), c(1, 4),
            c(2, 0), c(2, 1), c(2, 2), c(2, 3), c(2, 4), c(2, 5), c(2, 6),
            c(3, 0), c(3, 3), c(3, 6),
            c(4, 1), c(4, 2), c(4, 3), c(4, 4), c(4, 5),
            c(5, 2), c(5, 3), c(5, 4),
            c(6, 1), c(6, 4), c(6, 5),
      });
    note_cnt = 28;
    break;
  case 4:
    notes = (
             (uint32_t[])
      {
          c(0, 2), c(0, 4),
            c(1, 1), c(1, 3), c(1, 4), c(1, 5), c(1, 6),
            c(2, 0), c(2, 1), c(2, 3), c(2, 4), c(2, 6),
            c(3, 1), c(3, 4),
            c(4, 1), c(4, 2), c(4, 4), c(4, 6),
            c(5, 1), c(5, 3), c(5, 4), c(5, 6),
            c(6, 1), c(6, 4),
      });
    note_cnt = 24;
    break;
  default:
    notes = (uint32_t[]){};
    note_cnt = 0;
    break;
  }
  write_note(port_buf, i, notes, note_cnt, on);
}


void signal_handler(int sig)
{
	jack_client_close(client);
	fprintf(stderr, "signal received, exiting ...\n");
	exit(0);
}

void on_port_connect(jack_port_id_t a, jack_port_id_t b, int connect, void *args) {
  if (connect == 0)  { // connect
    printf("Disconnected!\n");
    return;
  }

  printf("Connected: %d <-> %d\n", a, b);
  peer = jack_port_by_id(client, b);
}

int process(jack_nframes_t nframes, void *arg) {
  void* port_buf = jack_port_get_buffer(output_port, nframes);
  jack_midi_clear_buffer(port_buf);

  for (int i = 0; i < nframes; i++) {
    loop_index %= 2 * sample_rate * 5;
    char_idx %= 6;
    if (loop_index % (2 * sample_rate) == 0) {
      printf("Writing note\n");
      write_char(port_buf, i, char_idx, 1);
    } else if (loop_index % (2 * sample_rate) == 2 * sample_rate - 1) {
      printf("Clearning note\n");
      write_char(port_buf, i, char_idx, 0);
      char_idx++;
    }
    loop_index++;
  }
  return 0;
}

void jack_shutdown(void *args) {
  exit(1);
}

int main() {
  const char **ports;
  const char *client_name = "xiaosi";
  jack_options_t options = JackNullOption;
  jack_status_t status;
  jack_client_t *client = jack_client_open(client_name, options, &status);
  if (client == NULL) {
		fprintf (stderr, "jack_client_open() failed, "
             "status = 0x%2.0x\n", status);
		if (status & JackServerFailed) {
			fprintf (stderr, "Unable to connect to JACK server\n");
		}
		exit (1);
	}
  if (status & JackServerStarted) {
		fprintf (stderr, "JACK server started\n");
	}
  if (status & JackNameNotUnique) {
		client_name = jack_get_client_name(client);
		fprintf (stderr, "unique name `%s' assigned\n", client_name);
	}
  jack_set_process_callback(client, process, 0);
  jack_on_shutdown (client, jack_shutdown, 0);
	output_port = jack_port_register (client, "output",
                                    JACK_DEFAULT_MIDI_TYPE,
                                    JackPortIsOutput, 0);

	if (output_port == NULL) {
		fprintf(stderr, "no more JACK ports available\n");
		exit (1);
	}

  jack_set_port_connect_callback(client, on_port_connect, NULL);

  if (jack_activate (client)) {
		fprintf (stderr, "cannot activate client");
		exit (1);
	}
  signal(SIGTERM, signal_handler);
	signal(SIGINT, signal_handler);
  sleep(-1);
  jack_client_close(client);
  exit(0);
}
