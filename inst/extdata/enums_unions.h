/* Test header with enums and unions */

/* Simple enum */
enum Color {
    RED = 0,
    GREEN = 1,
    BLUE = 2
};

/* Typedef enum */
typedef enum {
    STATUS_OK = 0,
    STATUS_ERROR = 1,
    STATUS_PENDING = 2,
    STATUS_CANCELLED = 3
} Status;

/* Enum with explicit values */
enum Flags {
    FLAG_NONE = 0,
    FLAG_READ = 1,
    FLAG_WRITE = 2,
    FLAG_EXECUTE = 4,
    FLAG_ALL = 7
};

/* Enum with gaps */
typedef enum {
    PRIORITY_LOW = 0,
    PRIORITY_MEDIUM = 5,
    PRIORITY_HIGH = 10,
    PRIORITY_CRITICAL = 100
} Priority;

/* Simple union */
union Value {
    int as_int;
    float as_float;
    double as_double;
};

/* Typedef union */
typedef union {
    int i;
    float f;
    char c[4];
} Data;

/* Union with different sized types */
typedef union {
    uint8_t byte;
    uint16_t word;
    uint32_t dword;
    uint64_t qword;
} IntUnion;

/* Struct with enum field */
typedef struct {
    enum Color color;
    int intensity;
} Pixel;

/* Struct with union field */
typedef struct {
    int tag;  /* 0=int, 1=float, 2=double */
    union Value value;
} TaggedValue;

/* Struct with typedef enum */
typedef struct {
    Status status;
    int error_code;
    char message[256];
} Result;

/* Complex: struct with both enum and union */
typedef struct {
    Priority priority;
    Data data;
    enum Flags flags;
} Task;

/* Function using enum parameter */
int process_color(enum Color c);

/* Function using enum return */
Status get_status(int id);

/* Function using union parameter */
void set_value(union Value* v);

/* Function using union return */
Data create_data(int i);

/* Function using struct with enum */
Pixel create_pixel(enum Color c, int intensity);

/* Function using struct with union */
TaggedValue create_tagged_int(int value);
TaggedValue create_tagged_float(float value);
