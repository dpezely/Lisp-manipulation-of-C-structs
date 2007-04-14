/* read-file.c - send UDP datagram to remote application */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>

static char *APP_NAME = "OTHER APP";
static char *APP_STATS_FILENAME = "payload.txt";

static char *STRUCTURED_FILENAME = "data.struct";

static unsigned char MAGIC_NUMBER = (unsigned char)'C';
static unsigned char PROTOCOL_REVISION = (unsigned char)241;
static unsigned char DEFAULT_FLAGS = (unsigned char)0x00;
static unsigned char DEFAULT_STATE = (unsigned char)0x23;

#define APP_NAME_LENGTH 16

//#define STATUS_LENGTH 40
#define STATUS_LENGTH 400

typedef struct _message {
  unsigned char magic_number;
  unsigned char revision;
  unsigned char flags;
  unsigned char state;

  char app_name[APP_NAME_LENGTH]; // null terminated or clipped

  unsigned short agent_id;	// zero or number of instance on node
  unsigned short other;		// UNUSED; for proper byte alignment

  // all time values are in seconds and in network byte order
  uint32_t node_time;		// sender's "now"
  uint32_t app_time;		// e.g., start of App

  char status[STATUS_LENGTH];	// stats from App
} message_t;

static int MAX_MESSAGE_SIZE = sizeof(message_t);

int parse_args(int argc, char **argv) {
  if (argc > 1) {
    int remaining = argc - 1;
    char **arg = &(argv[1]);
    char *next;
    while (remaining > 0) {
      if (remaining > 1) next = arg[1];
      else next = (char*)NULL;

      if (strcmp(*arg,"--app-name")==0  || strcmp(*arg,"-a")==0)
	if (next) { APP_NAME=next; ++arg; --remaining; }
	else usage(*arg);

      else if (strcmp(*arg,"--stats-file")==0  || strcmp(*arg,"-s")==0)
	if (next) { APP_STATS_FILENAME=next; ++arg; --remaining; }
	else usage(*arg);

      else if (strcmp(*arg,"--file")==0  || strcmp(*arg,"-f")==0)
	if (next) { STRUCTURED_FILENAME=next; ++arg; --remaining; }
	else usage(*arg);

      else usage(*arg);

      ++arg;
      --remaining;
    }
  }
}

int usage(const char *arg) {
  if (arg != (char*)NULL)
    fprintf(stderr,"\nIncorrect use of %s\n\n", arg);

  fprintf(stderr,"Flags: \n"
	  "-a --app-name '%s'\n"
	  "-s --stats-file '%s'\n"
	  "-f --file '%s'\n",
	  APP_NAME, APP_STATS_FILENAME, STRUCTURED_FILENAME);
  exit(1);
}

int show_state(void) {
  printf("App name: %s\n", APP_NAME);
  printf("App stats file: %s\n", APP_STATS_FILENAME);
  printf("Structured file to read: %s\n", STRUCTURED_FILENAME);

  printf("sizeof(char) is %d bytes\n", sizeof(char));
  printf("sizeof(short) is %d bytes\n", sizeof(short));
  printf("sizeof(timeval.tv_sec) is %d bytes\n", sizeof(time_t));
  printf("sizeof(uint32_t) is %d bytes\n", sizeof(uint32_t));

  message_t example;
  printf("offet of .app_name is byte %lu\n",
	 (unsigned long)&example.app_name - (unsigned long)&example);
  printf("offet of .agent_id is byte %lu\n",
	 (unsigned long)&example.agent_id - (unsigned long)&example);
  printf("offet of .node_time is byte %lu\n",
	 (unsigned long)&example.node_time - (unsigned long)&example);
  printf("offet of .app_time is byte %lu\n",
	 (unsigned long)&example.app_time - (unsigned long)&example);
  printf("offet of .status is byte %lu\n",
	 (unsigned long)&example.status - (unsigned long)&example);
}

int main(int argc, char *argv[]) {
  parse_args(argc, argv);
  show_state();

  // load struct from file:
  message_t message;
  bzero(&message, MAX_MESSAGE_SIZE);
  int fd = open(STRUCTURED_FILENAME, O_RDONLY);
  if (fd < 0) {
    perror("unable to load structured data file");
  } else {
    ssize_t loaded = read(fd, &message, MAX_MESSAGE_SIZE);
    close(fd);
    printf("loaded %ld bytes\n", (long)loaded);
  }
  printf("RAW BUFFER = ");
  int i;
  for (i=0; i!=sizeof(message); ++i) printf(" %.2d", ((unsigned char*)&message)[i]);
  printf("\n\n");
  printf("RAW BUFFER=0x");
  for (i=0; i!=sizeof(message); ++i) printf(" %.2X", ((unsigned char*)&message)[i]);
  printf("\n\n");
  printf("RAW BUFFER = \"%s...\"\n\n", (const char*)&message);

  printf("magic number = %d 0x%X '%c'\n",
	 message.magic_number, message.magic_number, message.magic_number);
  printf("revision = %d 0x%X '%c'\n", message.revision, message.revision, message.revision);
  printf("flags = %d 0x%X '%c'\n", message.flags, message.flags, message.flags);
  printf("state = %d 0x%X '%c'\n", message.state, message.state, message.state);

  printf("app name = \"%s\"\n", message.app_name);
  printf("agent id = %d 0x%X (net:%x)\n",
	 ntohs(message.agent_id), ntohs(message.agent_id), message.agent_id);

  time_t node_time = ntohl(message.node_time);
  printf("node time: %ld==0x%lX (net:%lx) %s\n",
	 node_time, node_time, message.node_time, ctime(&node_time));

  time_t app_time = ntohl(message.app_time);
  printf("app time: %ld==0x%lX (net:%lx) %s\n",
	 app_time, app_time, message.app_time, ctime(&app_time));

  printf("status = \"%s\"\n", message.status);
}
