/* Test header mixing structs, enums, and unions */

/* Enums for state machine */
typedef enum {
    STATE_IDLE = 0,
    STATE_RUNNING = 1,
    STATE_PAUSED = 2,
    STATE_STOPPED = 3
} MachineState;

typedef enum {
    EVENT_START = 0,
    EVENT_STOP = 1,
    EVENT_PAUSE = 2,
    EVENT_RESUME = 3
} MachineEvent;

/* Union for different event data */
typedef union {
    int timer_ms;
    char command[32];
    struct {
        int x;
        int y;
    } position;
} EventData;

/* Event with union and enum */
typedef struct {
    MachineEvent type;
    EventData data;
    unsigned long timestamp;
} Event;

/* State machine with nested types */
typedef struct {
    MachineState current_state;
    MachineState previous_state;
    Event last_event;
    int transition_count;
} StateMachine;

/* Enum for network protocol */
enum Protocol {
    PROTO_TCP = 0,
    PROTO_UDP = 1,
    PROTO_ICMP = 2
};

/* Union for network address */
typedef union {
    uint32_t ipv4;
    uint8_t ipv6[16];
} Address;

/* Network packet structure */
typedef struct {
    enum Protocol protocol;
    Address source;
    Address dest;
    uint16_t port;
    uint32_t seq_num;
} Packet;

/* Functions */
StateMachine* create_state_machine(void);
int transition_state(StateMachine* sm, MachineEvent event);
Event create_event(MachineEvent type, int data);
Packet create_packet(enum Protocol proto, uint32_t src, uint32_t dst, uint16_t port);
