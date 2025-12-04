// Test packed struct attributes

struct __attribute__((packed)) PackedStruct {
  char a;
  int b;
  char c;
};

typedef struct __attribute__((__packed__)) {
  char x;
  long y;
} PackedTypedef;

// Non-packed for comparison
struct NormalStruct {
  char a;
  int b;
  char c;
};
