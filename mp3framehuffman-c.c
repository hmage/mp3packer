#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

#include <caml/unixsupport.h>

#include "ptr.h"


// pulled out of the LAME decoder source huffman.h
// everything I've read says multi-bit lookup tables are faster than single-bit, but these tables look rather... intriguing

static const short tab0[] = { 
   0
};

static const short tab1[] = {
  -5,  -3,  -1,  17,   1,  16,   0
};

static const short tab2[] = {
 -15, -11,  -9,  -5,  -3,  -1,  34,   2,  18,  -1,  33,  32,  17,  -1,   1,
  16,   0
};

static const short tab3[] = {
 -13, -11,  -9,  -5,  -3,  -1,  34,   2,  18,  -1,  33,  32,  16,  17,  -1,
   1,   0
};

static const short tab5[] = {
 -29, -25, -23, -15,  -7,  -5,  -3,  -1,  51,  35,  50,  49,  -3,  -1,  19,
   3,  -1,  48,  34,  -3,  -1,  18,  33,  -1,   2,  32,  17,  -1,   1,  16,
   0
};

static const short tab6[] = {
 -25, -19, -13,  -9,  -5,  -3,  -1,  51,   3,  35,  -1,  50,  48,  -1,  19,
  49,  -3,  -1,  34,   2,  18,  -3,  -1,  33,  32,   1,  -1,  17,  -1,  16,
   0
};

static const short tab7[] = {
 -69, -65, -57, -39, -29, -17, -11,  -7,  -3,  -1,  85,  69,  -1,  84,  83,
  -1,  53,  68,  -3,  -1,  37,  82,  21,  -5,  -1,  81,  -1,   5,  52,  -1,
  80,  -1,  67,  51,  -5,  -3,  -1,  36,  66,  20,  -1,  65,  64, -11,  -7,
  -3,  -1,   4,  35,  -1,  50,   3,  -1,  19,  49,  -3,  -1,  48,  34,  18,
  -5,  -1,  33,  -1,   2,  32,  17,  -1,   1,  16,   0
};

static const short tab8[] = {
 -65, -63, -59, -45, -31, -19, -13,  -7,  -5,  -3,  -1,  85,  84,  69,  83,
  -3,  -1,  53,  68,  37,  -3,  -1,  82,   5,  21,  -5,  -1,  81,  -1,  52,
  67,  -3,  -1,  80,  51,  36,  -5,  -3,  -1,  66,  20,  65,  -3,  -1,   4,
  64,  -1,  35,  50,  -9,  -7,  -3,  -1,  19,  49,  -1,   3,  48,  34,  -1,
   2,  32,  -1,  18,  33,  17,  -3,  -1,   1,  16,   0
};

static const short tab9[] = {
 -63, -53, -41, -29, -19, -11,  -5,  -3,  -1,  85,  69,  53,  -1,  83,  -1,
  84,   5,  -3,  -1,  68,  37,  -1,  82,  21,  -3,  -1,  81,  52,  -1,  67,
  -1,  80,   4,  -7,  -3,  -1,  36,  66,  -1,  51,  64,  -1,  20,  65,  -5,
  -3,  -1,  35,  50,  19,  -1,  49,  -1,   3,  48,  -5,  -3,  -1,  34,   2,
  18,  -1,  33,  32,  -3,  -1,  17,   1,  -1,  16,   0
};

static const short tab10[] = {
-125,-121,-111, -83, -55, -35, -21, -13,  -7,  -3,  -1, 119, 103,  -1, 118,
  87,  -3,  -1, 117, 102,  71,  -3,  -1, 116,  86,  -1, 101,  55,  -9,  -3,
  -1, 115,  70,  -3,  -1,  85,  84,  99,  -1,  39, 114, -11,  -5,  -3,  -1,
 100,   7, 112,  -1,  98,  -1,  69,  53,  -5,  -1,   6,  -1,  83,  68,  23,
 -17,  -5,  -1, 113,  -1,  54,  38,  -5,  -3,  -1,  37,  82,  21,  -1,  81,
  -1,  52,  67,  -3,  -1,  22,  97,  -1,  96,  -1,   5,  80, -19, -11,  -7,
  -3,  -1,  36,  66,  -1,  51,   4,  -1,  20,  65,  -3,  -1,  64,  35,  -1,
  50,   3,  -3,  -1,  19,  49,  -1,  48,  34,  -7,  -3,  -1,  18,  33,  -1,
   2,  32,  17,  -1,   1,  16,   0
};

static const short tab11[] = {
-121,-113, -89, -59, -43, -27, -17,  -7,  -3,  -1, 119, 103,  -1, 118, 117,
  -3,  -1, 102,  71,  -1, 116,  -1,  87,  85,  -5,  -3,  -1,  86, 101,  55,
  -1, 115,  70,  -9,  -7,  -3,  -1,  69,  84,  -1,  53,  83,  39,  -1, 114,
  -1, 100,   7,  -5,  -1, 113,  -1,  23, 112,  -3,  -1,  54,  99,  -1,  96,
  -1,  68,  37, -13,  -7,  -5,  -3,  -1,  82,   5,  21,  98,  -3,  -1,  38,
   6,  22,  -5,  -1,  97,  -1,  81,  52,  -5,  -1,  80,  -1,  67,  51,  -1,
  36,  66, -15, -11,  -7,  -3,  -1,  20,  65,  -1,   4,  64,  -1,  35,  50,
  -1,  19,  49,  -5,  -3,  -1,   3,  48,  34,  33,  -5,  -1,  18,  -1,   2,
  32,  17,  -3,  -1,   1,  16,   0
};

static const short tab12[] = {
-115, -99, -73, -45, -27, -17,  -9,  -5,  -3,  -1, 119, 103, 118,  -1,  87,
 117,  -3,  -1, 102,  71,  -1, 116, 101,  -3,  -1,  86,  55,  -3,  -1, 115,
  85,  39,  -7,  -3,  -1, 114,  70,  -1, 100,  23,  -5,  -1, 113,  -1,   7,
 112,  -1,  54,  99, -13,  -9,  -3,  -1,  69,  84,  -1,  68,  -1,   6,   5,
  -1,  38,  98,  -5,  -1,  97,  -1,  22,  96,  -3,  -1,  53,  83,  -1,  37,
  82, -17,  -7,  -3,  -1,  21,  81,  -1,  52,  67,  -5,  -3,  -1,  80,   4,
  36,  -1,  66,  20,  -3,  -1,  51,  65,  -1,  35,  50, -11,  -7,  -5,  -3,
  -1,  64,   3,  48,  19,  -1,  49,  34,  -1,  18,  33,  -7,  -5,  -3,  -1,
   2,  32,   0,  17,  -1,   1,  16
};

static const short tab13[] = {
-509,-503,-475,-405,-333,-265,-205,-153,-115, -83, -53, -35, -21, -13,  -9,
  -7,  -5,  -3,  -1, 254, 252, 253, 237, 255,  -1, 239, 223,  -3,  -1, 238,
 207,  -1, 222, 191,  -9,  -3,  -1, 251, 206,  -1, 220,  -1, 175, 233,  -1,
 236, 221,  -9,  -5,  -3,  -1, 250, 205, 190,  -1, 235, 159,  -3,  -1, 249,
 234,  -1, 189, 219, -17,  -9,  -3,  -1, 143, 248,  -1, 204,  -1, 174, 158,
  -5,  -1, 142,  -1, 127, 126, 247,  -5,  -1, 218,  -1, 173, 188,  -3,  -1,
 203, 246, 111, -15,  -7,  -3,  -1, 232,  95,  -1, 157, 217,  -3,  -1, 245,
 231,  -1, 172, 187,  -9,  -3,  -1,  79, 244,  -3,  -1, 202, 230, 243,  -1,
  63,  -1, 141, 216, -21,  -9,  -3,  -1,  47, 242,  -3,  -1, 110, 156,  15,
  -5,  -3,  -1, 201,  94, 171,  -3,  -1, 125, 215,  78, -11,  -5,  -3,  -1,
 200, 214,  62,  -1, 185,  -1, 155, 170,  -1,  31, 241, -23, -13,  -5,  -1,
 240,  -1, 186, 229,  -3,  -1, 228, 140,  -1, 109, 227,  -5,  -1, 226,  -1,
  46,  14,  -1,  30, 225, -15,  -7,  -3,  -1, 224,  93,  -1, 213, 124,  -3,
  -1, 199,  77,  -1, 139, 184,  -7,  -3,  -1, 212, 154,  -1, 169, 108,  -1,
 198,  61, -37, -21,  -9,  -5,  -3,  -1, 211, 123,  45,  -1, 210,  29,  -5,
  -1, 183,  -1,  92, 197,  -3,  -1, 153, 122, 195,  -7,  -5,  -3,  -1, 167,
 151,  75, 209,  -3,  -1,  13, 208,  -1, 138, 168, -11,  -7,  -3,  -1,  76,
 196,  -1, 107, 182,  -1,  60,  44,  -3,  -1, 194,  91,  -3,  -1, 181, 137,
  28, -43, -23, -11,  -5,  -1, 193,  -1, 152,  12,  -1, 192,  -1, 180, 106,
  -5,  -3,  -1, 166, 121,  59,  -1, 179,  -1, 136,  90, -11,  -5,  -1,  43,
  -1, 165, 105,  -1, 164,  -1, 120, 135,  -5,  -1, 148,  -1, 119, 118, 178,
 -11,  -3,  -1,  27, 177,  -3,  -1,  11, 176,  -1, 150,  74,  -7,  -3,  -1,
  58, 163,  -1,  89, 149,  -1,  42, 162, -47, -23,  -9,  -3,  -1,  26, 161,
  -3,  -1,  10, 104, 160,  -5,  -3,  -1, 134,  73, 147,  -3,  -1,  57,  88,
  -1, 133, 103,  -9,  -3,  -1,  41, 146,  -3,  -1,  87, 117,  56,  -5,  -1,
 131,  -1, 102,  71,  -3,  -1, 116,  86,  -1, 101, 115, -11,  -3,  -1,  25,
 145,  -3,  -1,   9, 144,  -1,  72, 132,  -7,  -5,  -1, 114,  -1,  70, 100,
  40,  -1, 130,  24, -41, -27, -11,  -5,  -3,  -1,  55,  39,  23,  -1, 113,
  -1,  85,   7,  -7,  -3,  -1, 112,  54,  -1,  99,  69,  -3,  -1,  84,  38,
  -1,  98,  53,  -5,  -1, 129,  -1,   8, 128,  -3,  -1,  22,  97,  -1,   6,
  96, -13,  -9,  -5,  -3,  -1,  83,  68,  37,  -1,  82,   5,  -1,  21,  81,
  -7,  -3,  -1,  52,  67,  -1,  80,  36,  -3,  -1,  66,  51,  20, -19, -11,
  -5,  -1,  65,  -1,   4,  64,  -3,  -1,  35,  50,  19,  -3,  -1,  49,   3,
  -1,  48,  34,  -3,  -1,  18,  33,  -1,   2,  32,  -3,  -1,  17,   1,  16,
   0
};

static const short tab15[] = {
-495,-445,-355,-263,-183,-115, -77, -43, -27, -13,  -7,  -3,  -1, 255, 239,
  -1, 254, 223,  -1, 238,  -1, 253, 207,  -7,  -3,  -1, 252, 222,  -1, 237,
 191,  -1, 251,  -1, 206, 236,  -7,  -3,  -1, 221, 175,  -1, 250, 190,  -3,
  -1, 235, 205,  -1, 220, 159, -15,  -7,  -3,  -1, 249, 234,  -1, 189, 219,
  -3,  -1, 143, 248,  -1, 204, 158,  -7,  -3,  -1, 233, 127,  -1, 247, 173,
  -3,  -1, 218, 188,  -1, 111,  -1, 174,  15, -19, -11,  -3,  -1, 203, 246,
  -3,  -1, 142, 232,  -1,  95, 157,  -3,  -1, 245, 126,  -1, 231, 172,  -9,
  -3,  -1, 202, 187,  -3,  -1, 217, 141,  79,  -3,  -1, 244,  63,  -1, 243,
 216, -33, -17,  -9,  -3,  -1, 230,  47,  -1, 242,  -1, 110, 240,  -3,  -1,
  31, 241,  -1, 156, 201,  -7,  -3,  -1,  94, 171,  -1, 186, 229,  -3,  -1,
 125, 215,  -1,  78, 228, -15,  -7,  -3,  -1, 140, 200,  -1,  62, 109,  -3,
  -1, 214, 227,  -1, 155, 185,  -7,  -3,  -1,  46, 170,  -1, 226,  30,  -5,
  -1, 225,  -1,  14, 224,  -1,  93, 213, -45, -25, -13,  -7,  -3,  -1, 124,
 199,  -1,  77, 139,  -1, 212,  -1, 184, 154,  -7,  -3,  -1, 169, 108,  -1,
 198,  61,  -1, 211, 210,  -9,  -5,  -3,  -1,  45,  13,  29,  -1, 123, 183,
  -5,  -1, 209,  -1,  92, 208,  -1, 197, 138, -17,  -7,  -3,  -1, 168,  76,
  -1, 196, 107,  -5,  -1, 182,  -1, 153,  12,  -1,  60, 195,  -9,  -3,  -1,
 122, 167,  -1, 166,  -1, 192,  11,  -1, 194,  -1,  44,  91, -55, -29, -15,
  -7,  -3,  -1, 181,  28,  -1, 137, 152,  -3,  -1, 193,  75,  -1, 180, 106,
  -5,  -3,  -1,  59, 121, 179,  -3,  -1, 151, 136,  -1,  43,  90, -11,  -5,
  -1, 178,  -1, 165,  27,  -1, 177,  -1, 176, 105,  -7,  -3,  -1, 150,  74,
  -1, 164, 120,  -3,  -1, 135,  58, 163, -17,  -7,  -3,  -1,  89, 149,  -1,
  42, 162,  -3,  -1,  26, 161,  -3,  -1,  10, 160, 104,  -7,  -3,  -1, 134,
  73,  -1, 148,  57,  -5,  -1, 147,  -1, 119,   9,  -1,  88, 133, -53, -29,
 -13,  -7,  -3,  -1,  41, 103,  -1, 118, 146,  -1, 145,  -1,  25, 144,  -7,
  -3,  -1,  72, 132,  -1,  87, 117,  -3,  -1,  56, 131,  -1, 102,  71,  -7,
  -3,  -1,  40, 130,  -1,  24, 129,  -7,  -3,  -1, 116,   8,  -1, 128,  86,
  -3,  -1, 101,  55,  -1, 115,  70, -17,  -7,  -3,  -1,  39, 114,  -1, 100,
  23,  -3,  -1,  85, 113,  -3,  -1,   7, 112,  54,  -7,  -3,  -1,  99,  69,
  -1,  84,  38,  -3,  -1,  98,  22,  -3,  -1,   6,  96,  53, -33, -19,  -9,
  -5,  -1,  97,  -1,  83,  68,  -1,  37,  82,  -3,  -1,  21,  81,  -3,  -1,
   5,  80,  52,  -7,  -3,  -1,  67,  36,  -1,  66,  51,  -1,  65,  -1,  20,
   4,  -9,  -3,  -1,  35,  50,  -3,  -1,  64,   3,  19,  -3,  -1,  49,  48,
  34,  -9,  -7,  -3,  -1,  18,  33,  -1,   2,  32,  17,  -3,  -1,   1,  16,
   0
};

static const short tab16[] = {
-509,-503,-461,-323,-103, -37, -27, -15,  -7,  -3,  -1, 239, 254,  -1, 223,
 253,  -3,  -1, 207, 252,  -1, 191, 251,  -5,  -1, 175,  -1, 250, 159,  -3,
  -1, 249, 248, 143,  -7,  -3,  -1, 127, 247,  -1, 111, 246, 255,  -9,  -5,
  -3,  -1,  95, 245,  79,  -1, 244, 243, -53,  -1, 240,  -1,  63, -29, -19,
 -13,  -7,  -5,  -1, 206,  -1, 236, 221, 222,  -1, 233,  -1, 234, 217,  -1,
 238,  -1, 237, 235,  -3,  -1, 190, 205,  -3,  -1, 220, 219, 174, -11,  -5,
  -1, 204,  -1, 173, 218,  -3,  -1, 126, 172, 202,  -5,  -3,  -1, 201, 125,
  94, 189, 242, -93,  -5,  -3,  -1,  47,  15,  31,  -1, 241, -49, -25, -13,
  -5,  -1, 158,  -1, 188, 203,  -3,  -1, 142, 232,  -1, 157, 231,  -7,  -3,
  -1, 187, 141,  -1, 216, 110,  -1, 230, 156, -13,  -7,  -3,  -1, 171, 186,
  -1, 229, 215,  -1,  78,  -1, 228, 140,  -3,  -1, 200,  62,  -1, 109,  -1,
 214, 155, -19, -11,  -5,  -3,  -1, 185, 170, 225,  -1, 212,  -1, 184, 169,
  -5,  -1, 123,  -1, 183, 208, 227,  -7,  -3,  -1,  14, 224,  -1,  93, 213,
  -3,  -1, 124, 199,  -1,  77, 139, -75, -45, -27, -13,  -7,  -3,  -1, 154,
 108,  -1, 198,  61,  -3,  -1,  92, 197,  13,  -7,  -3,  -1, 138, 168,  -1,
 153,  76,  -3,  -1, 182, 122,  60, -11,  -5,  -3,  -1,  91, 137,  28,  -1,
 192,  -1, 152, 121,  -1, 226,  -1,  46,  30, -15,  -7,  -3,  -1, 211,  45,
  -1, 210, 209,  -5,  -1,  59,  -1, 151, 136,  29,  -7,  -3,  -1, 196, 107,
  -1, 195, 167,  -1,  44,  -1, 194, 181, -23, -13,  -7,  -3,  -1, 193,  12,
  -1,  75, 180,  -3,  -1, 106, 166, 179,  -5,  -3,  -1,  90, 165,  43,  -1,
 178,  27, -13,  -5,  -1, 177,  -1,  11, 176,  -3,  -1, 105, 150,  -1,  74,
 164,  -5,  -3,  -1, 120, 135, 163,  -3,  -1,  58,  89,  42, -97, -57, -33,
 -19, -11,  -5,  -3,  -1, 149, 104, 161,  -3,  -1, 134, 119, 148,  -5,  -3,
  -1,  73,  87, 103, 162,  -5,  -1,  26,  -1,  10, 160,  -3,  -1,  57, 147,
  -1,  88, 133,  -9,  -3,  -1,  41, 146,  -3,  -1, 118,   9,  25,  -5,  -1,
 145,  -1, 144,  72,  -3,  -1, 132, 117,  -1,  56, 131, -21, -11,  -5,  -3,
  -1, 102,  40, 130,  -3,  -1,  71, 116,  24,  -3,  -1, 129, 128,  -3,  -1,
   8,  86,  55,  -9,  -5,  -1, 115,  -1, 101,  70,  -1,  39, 114,  -5,  -3,
  -1, 100,  85,   7,  23, -23, -13,  -5,  -1, 113,  -1, 112,  54,  -3,  -1,
  99,  69,  -1,  84,  38,  -3,  -1,  98,  22,  -1,  97,  -1,   6,  96,  -9,
  -5,  -1,  83,  -1,  53,  68,  -1,  37,  82,  -1,  81,  -1,  21,   5, -33,
 -23, -13,  -7,  -3,  -1,  52,  67,  -1,  80,  36,  -3,  -1,  66,  51,  20,
  -5,  -1,  65,  -1,   4,  64,  -1,  35,  50,  -3,  -1,  19,  49,  -3,  -1,
   3,  48,  34,  -3,  -1,  18,  33,  -1,   2,  32,  -3,  -1,  17,   1,  16,
   0
};

static const short tab24[] = {
-451,-117, -43, -25, -15,  -7,  -3,  -1, 239, 254,  -1, 223, 253,  -3,  -1,
 207, 252,  -1, 191, 251,  -5,  -1, 250,  -1, 175, 159,  -1, 249, 248,  -9,
  -5,  -3,  -1, 143, 127, 247,  -1, 111, 246,  -3,  -1,  95, 245,  -1,  79,
 244, -71,  -7,  -3,  -1,  63, 243,  -1,  47, 242,  -5,  -1, 241,  -1,  31,
 240, -25,  -9,  -1,  15,  -3,  -1, 238, 222,  -1, 237, 206,  -7,  -3,  -1,
 236, 221,  -1, 190, 235,  -3,  -1, 205, 220,  -1, 174, 234, -15,  -7,  -3,
  -1, 189, 219,  -1, 204, 158,  -3,  -1, 233, 173,  -1, 218, 188,  -7,  -3,
  -1, 203, 142,  -1, 232, 157,  -3,  -1, 217, 126,  -1, 231, 172, 255,-235,
-143, -77, -45, -25, -15,  -7,  -3,  -1, 202, 187,  -1, 141, 216,  -5,  -3,
  -1,  14, 224,  13, 230,  -5,  -3,  -1, 110, 156, 201,  -1,  94, 186,  -9,
  -5,  -1, 229,  -1, 171, 125,  -1, 215, 228,  -3,  -1, 140, 200,  -3,  -1,
  78,  46,  62, -15,  -7,  -3,  -1, 109, 214,  -1, 227, 155,  -3,  -1, 185,
 170,  -1, 226,  30,  -7,  -3,  -1, 225,  93,  -1, 213, 124,  -3,  -1, 199,
  77,  -1, 139, 184, -31, -15,  -7,  -3,  -1, 212, 154,  -1, 169, 108,  -3,
  -1, 198,  61,  -1, 211,  45,  -7,  -3,  -1, 210,  29,  -1, 123, 183,  -3,
  -1, 209,  92,  -1, 197, 138, -17,  -7,  -3,  -1, 168, 153,  -1,  76, 196,
  -3,  -1, 107, 182,  -3,  -1, 208,  12,  60,  -7,  -3,  -1, 195, 122,  -1,
 167,  44,  -3,  -1, 194,  91,  -1, 181,  28, -57, -35, -19,  -7,  -3,  -1,
 137, 152,  -1, 193,  75,  -5,  -3,  -1, 192,  11,  59,  -3,  -1, 176,  10,
  26,  -5,  -1, 180,  -1, 106, 166,  -3,  -1, 121, 151,  -3,  -1, 160,   9,
 144,  -9,  -3,  -1, 179, 136,  -3,  -1,  43,  90, 178,  -7,  -3,  -1, 165,
  27,  -1, 177, 105,  -1, 150, 164, -17,  -9,  -5,  -3,  -1,  74, 120, 135,
  -1,  58, 163,  -3,  -1,  89, 149,  -1,  42, 162,  -7,  -3,  -1, 161, 104,
  -1, 134, 119,  -3,  -1,  73, 148,  -1,  57, 147, -63, -31, -15,  -7,  -3,
  -1,  88, 133,  -1,  41, 103,  -3,  -1, 118, 146,  -1,  25, 145,  -7,  -3,
  -1,  72, 132,  -1,  87, 117,  -3,  -1,  56, 131,  -1, 102,  40, -17,  -7,
  -3,  -1, 130,  24,  -1,  71, 116,  -5,  -1, 129,  -1,   8, 128,  -1,  86,
 101,  -7,  -5,  -1,  23,  -1,   7, 112, 115,  -3,  -1,  55,  39, 114, -15,
  -7,  -3,  -1,  70, 100,  -1,  85, 113,  -3,  -1,  54,  99,  -1,  69,  84,
  -7,  -3,  -1,  38,  98,  -1,  22,  97,  -5,  -3,  -1,   6,  96,  53,  -1,
  83,  68, -51, -37, -23, -15,  -9,  -3,  -1,  37,  82,  -1,  21,  -1,   5,
  80,  -1,  81,  -1,  52,  67,  -3,  -1,  36,  66,  -1,  51,  20,  -9,  -5,
  -1,  65,  -1,   4,  64,  -1,  35,  50,  -1,  19,  49,  -7,  -5,  -3,  -1,
   3,  48,  34,  18,  -1,  33,  -1,   2,  32,  -3,  -1,  17,   1,  -1,  16,
   0
};

static const short tab_c0[] = {
 -29, -21, -13,  -7,  -3,  -1,  11,  15,  -1,  13,  14,  -3,  -1,   7,   5,
   9,  -3,  -1,   6,   3,  -1,  10,  12,  -3,  -1,   2,   1,  -1,   4,   8,
   0
};

static const short tab_c1[] = {
 -15,  -7,  -3,  -1,  15,  14,  -1,  13,  12,  -3,  -1,  11,  10,  -1,   9,
   8,  -7,  -3,  -1,   7,   6,  -1,   5,   4,  -3,  -1,   3,   2,  -1,   1,
   0
};


static const short * mfh_tables[] = {tab0, tab1, tab2, tab3, tab0, tab5, tab6, tab7, tab8, tab9, tab10, tab11, tab12, tab13, tab0, tab15, tab16, tab16, tab16, tab16, tab16, tab16, tab16, tab16, tab24, tab24, tab24, tab24, tab24, tab24, tab24, tab24, tab_c0, tab_c1};

//int get_one_bit(int val, int shift) {
//	return((val >> shift) & 1);
//}

// Don't bother with CAMLparam/CAMLreturn, since everything that OCaml deals with here is an int
// Actually, there is a bit of a problem in that the returned value needs to communicate the number of bits read...
// in order to not have to use anything on the OCaml heap, we need to encode it into the returned value
// The returned value in this function is therefore:
// (ret >> 4) & 15 = x
// (ret     ) & 15 = y
// (ret >> 8)      = num bits used
// Also, in order to reuse the same function, I've stuck the part1 tables on the end of the bigvalue tables
CAMLprim value mfh_get_huffman(value ht_val, value from_val, value start_shift_val) {
	int ht = Int_val(ht_val);
	int from = Int_val(from_val);
	int start_shift = Int_val(start_shift_val);
	int shift = start_shift;
	const short *tab = mfh_tables[ht];
	short got;

	while((got = *tab++) < 0) {
		if((from >> shift) & 1)
			tab -= got;
		shift--;
	}
	return(Val_int(got | ((start_shift - shift) << 8)));
}


static const short linbits_table[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,6,8,10,13,4,5,6,7,8,9,11,13};


typedef struct {
	unsigned char *start_byte_ptr;
	unsigned char *byte_ptr;
	int bit_index;
} seq_string_t;

static unsigned int get_1_bit(seq_string_t *s) {
	unsigned char ret = *(s->byte_ptr) << s->bit_index;

	s->bit_index++;
	s->byte_ptr += (s->bit_index >> 3);
	s->bit_index &= 7;

//	printf("Diff is %d\n", s->byte_ptr - s->start_byte_ptr);

	return(ret >> 7);
}

static unsigned int get_bits_slow(seq_string_t *s, int num_bits) {
	unsigned int ret = 0;
	int i;
	for(i = 0; i < num_bits; i++) {
		ret <<= 1;
		ret |= get_1_bit(s);
	}
	return ret;
}

static unsigned int get_bits_wordwise(seq_string_t *s, int num_bits) {
	uint32 *int_ptr;
	uint32 raw;
	int_ptr = (uint32 *)s->byte_ptr;

#if 1
	raw = _byteswap_ulong(*int_ptr) << s->bit_index;
#else
	raw = (
		(s->byte_ptr[0] << 24) |
		(s->byte_ptr[1] << 16) |
		(s->byte_ptr[2] <<  8) |
		(s->byte_ptr[3]      )
	);
#endif
	raw >>= 1;
	raw >>= 31 - num_bits;

	s->bit_index += num_bits;
	s->byte_ptr += (s->bit_index >> 3);
	s->bit_index &= 7;

	return raw;
}

unsigned int bit_off(seq_string_t *s) {
	return((s->byte_ptr - s->start_byte_ptr) * 8 + s->bit_index);
}

// Decodes starting from bit in_off_val and quant out_off_val, using table table_val
// Try to make a ptr that can store an extra 36 bits (that's the max length for a single quant value)
// The output ptr should have space for at least 576 ints

static int decode_big_quants(
	seq_string_t *s,
	int16_t *out_quants,
	int in_off_too_many,
	int out_off,
	int out_off_too_many,
	int ht
) {
	const short *start_tab = mfh_tables[ht];
	const short *tab;
	short linbits = linbits_table[ht];

	while(out_off < out_off_too_many && (bit_off(s) < in_off_too_many || ht == 0)) {
		short got;
		int x, y;
		tab = start_tab;
		while((got = *tab++) < 0) {
			if(get_1_bit(s)) {
				tab -= got;
			}
		}
		y = (got & 15);
		x = got >> 4;
		if(x > 0) {
			if(x == 15) {
				x += get_bits_wordwise(s, linbits);
			}
			x *= 1 - 2 * get_1_bit(s);
		}
		if(y > 0) {
			if(y == 15) {
				y += get_bits_wordwise(s, linbits);
			}
			y *= 1 - 2 * get_1_bit(s);
		}
		out_quants[out_off++] = x;
		out_quants[out_off++] = y;
	}
	return(out_off);
}

CAMLprim value mfh_decode_big_quants(
	value in_ptr_val,
	value in_off_val,
	value in_off_too_many_val,
	value out_ptr_val,
	value out_off_val,
	value out_off_too_many_val,
	value ht_val
) {
	CAMLparam5(in_ptr_val, in_off_val, in_off_too_many_val, out_ptr_val, out_off_val);
	CAMLxparam2(out_off_too_many_val, ht_val);
	unsigned char *in_chars = (unsigned char *)Begin_val(in_ptr_val);
	int in_off = Int_val(in_off_val);
	int in_off_too_many = Int_val(in_off_too_many_val);
	int16_t *out_quants = (int16_t *)Begin_val(out_ptr_val);
	int out_off = Int_val(out_off_val);
	int out_off_too_many = Int_val(out_off_too_many_val);
	int ht = Int_val(ht_val);
	const short *start_tab = mfh_tables[ht];
	const short *tab;
	short linbits = linbits_table[ht];
	seq_string_t s;

	CAMLlocal1(out_val);

	enter_blocking_section();

	s.start_byte_ptr = in_chars;
	s.byte_ptr = in_chars + (in_off >> 3);
	s.bit_index = (in_off & 7);

	out_off = decode_big_quants(&s, out_quants, in_off_too_many, out_off, out_off_too_many, ht);

	leave_blocking_section();

	out_val = caml_alloc_tuple(2);
	Store_field(out_val, 0, Val_int(bit_off(&s)));
	Store_field(out_val, 1, Val_int(out_off));

	CAMLreturn(out_val);
}

CAMLprim value mfh_decode_big_quants_bytecode(value *argv, int argn) {
	return mfh_decode_big_quants(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}


static int decode_all_big_quants(
	seq_string_t *s,
	int16_t *out_quants,
	int in_off_too_many,
	int count0,
	int ht0,
	int count1,
	int ht1,
	int count2,
	int ht2,
	int *error
) {

	int out_off = 0;
	int out_off_too_many;

	out_off_too_many = out_off + count0;
	out_off = decode_big_quants(s, out_quants, in_off_too_many, out_off, out_off_too_many, ht0);
	*error |= (out_off != out_off_too_many);
	out_off_too_many = out_off + count1;
	out_off = decode_big_quants(s, out_quants, in_off_too_many, out_off, out_off_too_many, ht1);
	*error |= (out_off != out_off_too_many);
	out_off_too_many = out_off + count2;
	out_off = decode_big_quants(s, out_quants, in_off_too_many, out_off, out_off_too_many, ht2);
	*error |= (out_off != out_off_too_many);

	return(out_off);
}

CAMLprim value mfh_decode_all_big_quants(
	value in_ptr_val,
	value in_off_val,
	value out_ptr_val,
	value in_off_too_many_val,
	value count0_val,
	value ht0_val,
	value count1_val,
	value ht1_val,
	value count2_val,
	value ht2_val
) {
	CAMLparam5(in_ptr_val, in_off_val, out_ptr_val, in_off_too_many_val, count0_val);
	CAMLxparam5(ht0_val, count1_val, ht1_val, count2_val, ht2_val);
	unsigned char *in_chars = (unsigned char *)Begin_val(in_ptr_val);
	int in_off = Int_val(in_off_val);
	int in_off_too_many = Int_val(in_off_too_many_val);
	int16_t *out_quants = (int16_t *)Begin_val(out_ptr_val);
	int count0 = Int_val(count0_val);
	int ht0 = Int_val(ht0_val);
	int count1 = Int_val(count1_val);
	int ht1 = Int_val(ht1_val);
	int count2 = Int_val(count2_val);
	int ht2 = Int_val(ht2_val);
	seq_string_t s;
	int out_off;
	int error = 0;

	CAMLlocal1(out_val);

	enter_blocking_section();

	s.start_byte_ptr = in_chars;
	s.byte_ptr = in_chars + (in_off >> 3);
	s.bit_index = (in_off & 7);

	out_off = decode_all_big_quants(&s, out_quants, in_off_too_many, count0, ht0, count1, ht1, count2, ht2, &error);

	leave_blocking_section();

	out_val = caml_alloc_tuple(2);
	Store_field(out_val, 0, Val_int(bit_off(&s)));
	Store_field(out_val, 1, Val_int(out_off));

	CAMLreturn(out_val);
}

CAMLprim value mfh_decode_all_big_quants_bytecode(value *argv, int argn) {
	return mfh_decode_all_big_quants(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9]);
}



static int decode_count1_quants(
	seq_string_t *s,
	int16_t *out_quants,
	int in_off_too_many,
	int out_off,
	int out_off_too_many,
	int ht_one,
	int *overboard
) {
	const short *start_tab = (ht_one ? tab_c1 : tab_c0);
	const short *tab;
	while(out_off <= out_off_too_many - 4 && bit_off(s) < in_off_too_many) {
		short got;
		int v = 0, w = 0, x = 0, y = 0;
		tab = start_tab;
		while((got = *tab++) < 0) {
			if(get_1_bit(s)) {
				tab -= got;
			}
		}
		if(got & 8) {
			v = 1 - 2 * get_1_bit(s);
		}
		if(got & 4) {
			w = 1 - 2 * get_1_bit(s);
		}
		if(got & 2) {
			x = 1 - 2 * get_1_bit(s);
		}
		if(got & 1) {
			y = 1 - 2 * get_1_bit(s);
		}

		out_quants[out_off++] = v;
		out_quants[out_off++] = w;
		out_quants[out_off++] = x;
		out_quants[out_off++] = y;
	}
//	printf("OUT_OFF = %d at middle\n", out_off);

	// Now check to see if this writes past the end
	if(out_off < out_off_too_many && bit_off(s) < in_off_too_many) {
		short got;
		int v = 0, w = 0, x = 0, y = 0;
		int any_nonzero = 0;
		tab = start_tab;
		while((got = *tab++) < 0) {
			if(get_1_bit(s)) {
				tab -= got;
			}
		}

		if(got & 8) {
			v = 1 - 2 * get_1_bit(s);
			any_nonzero |= v;
		}
		if(got & 4) {
			w = 1 - 2 * get_1_bit(s);
			any_nonzero |= w;
		}
		if(got & 2) {
			x = 1 - 2 * get_1_bit(s);
			any_nonzero |= x;
		}
		if(got & 1) {
			y = 1 - 2 * get_1_bit(s);
			any_nonzero |= y;
		}

		out_quants[out_off++] = v;
		if(out_off < out_off_too_many) {
			out_quants[out_off++] = w;
		} else if(any_nonzero) {
//			printf("OVERBOARD_C\n");
			*overboard = 1;
		}
		if(out_off < out_off_too_many) {
			out_quants[out_off++] = x;
		} else if(any_nonzero) {
//			printf("OVERBOARD_C (%d)\n", out_quants[out_off_too_many - 2]);
			*overboard = 1;
		}
		if(out_off < out_off_too_many) {
			out_quants[out_off++] = y;
		} else if(any_nonzero) {
//			printf("OVERBOARD_C (%d)\n", out_quants[out_off_too_many - 1]);
			*overboard = 1;
		}
	}
	return out_off; // Probably don't need this?
}

CAMLprim value mfh_decode_count1_quants(
	value in_ptr_val,
	value in_off_val,
	value in_off_too_many_val,
	value out_ptr_val,
	value out_off_val,
	value out_off_too_many_val,
	value ht_one_val
) {
	CAMLparam5(in_ptr_val, in_off_val, in_off_too_many_val, out_ptr_val, out_off_val);
	CAMLxparam2(out_off_too_many_val, ht_one_val);
//	CAMLparam2(in_ptr_val, out_ptr_val);
	unsigned char *in_chars = (unsigned char *)Begin_val(in_ptr_val);
	int in_off = Int_val(in_off_val);
	int in_off_too_many = Int_val(in_off_too_many_val);
	int16_t *out_quants = (int16_t *)Begin_val(out_ptr_val);
	int out_off = Int_val(out_off_val);
	int out_off_too_many = Int_val(out_off_too_many_val);
	int ht = Int_val(ht_one_val);
	const short *start_tab = (ht ? tab_c1 : tab_c0);
	const short *tab;
	int overboard = 0;
	seq_string_t s;

	CAMLlocal1(out_val);

	enter_blocking_section();

	s.start_byte_ptr = in_chars;
	s.byte_ptr = in_chars + (in_off >> 3);
	s.bit_index = (in_off & 7);


	out_off = decode_count1_quants(&s, out_quants, in_off_too_many, out_off, out_off_too_many, ht, &overboard);
/*
	while(out_off <= out_off_too_many - 4 && bit_off(&s) < in_off_too_many) {
		short got;
		int v = 0, w = 0, x = 0, y = 0;
		tab = start_tab;
		while((got = *tab++) < 0) {
			if(get_1_bit(&s)) {
				tab -= got;
			}
//			in_off++;
		}
/ *
		v = (got >> 3);
		w = (got >> 2) & 1;
		x = (got >> 1) & 1;
		y = (got     ) & 1;
* /
		if(got & 8) {
			v = 1 - 2 * get_1_bit(&s);
//			in_off++;
		}
		if(got & 4) {
			w = 1 - 2 * get_1_bit(&s);
//			in_off++;
		}
		if(got & 2) {
			x = 1 - 2 * get_1_bit(&s);
//			in_off++;
		}
		if(got & 1) {
			y = 1 - 2 * get_1_bit(&s);
//			in_off++;
		}

		out_quants[out_off++] = v;
		out_quants[out_off++] = w;
		out_quants[out_off++] = x;
		out_quants[out_off++] = y;
	}
//	printf("OUT_OFF = %d at middle\n", out_off);

	// Now check to see if this writes past the end
	if(out_off < out_off_too_many && bit_off(&s) < in_off_too_many) {
		short got;
		int v = 0, w = 0, x = 0, y = 0;
		int any_nonzero = 0;
		tab = start_tab;
		while((got = *tab++) < 0) {
			if(get_1_bit(&s)) {
				tab -= got;
			}
//			in_off++;
		}

		if(got & 8) {
			v = 1 - 2 * get_1_bit(&s);
			any_nonzero |= v;
//			in_off++;
		}
		if(got & 4) {
			w = 1 - 2 * get_1_bit(&s);
			any_nonzero |= w;
//			in_off++;
		}
		if(got & 2) {
			x = 1 - 2 * get_1_bit(&s);
			any_nonzero |= x;
//			in_off++;
		}
		if(got & 1) {
			y = 1 - 2 * get_1_bit(&s);
			any_nonzero |= y;
//			in_off++;
		}

		out_quants[out_off++] = v;
		if(out_off < out_off_too_many) {
			out_quants[out_off++] = w;
		} else if(any_nonzero) {
//			printf("OVERBOARD_B\n");
			overboard = 1;
		}
		if(out_off < out_off_too_many) {
			out_quants[out_off++] = x;
		} else if(any_nonzero) {
//			printf("OVERBOARD_B (%d)\n", out_quants[out_off_too_many - 2]);
			overboard = 1;
		}
		if(out_off < out_off_too_many) {
			out_quants[out_off++] = y;
		} else if(any_nonzero) {
//			printf("OVERBOARD_B (%d)\n", out_quants[out_off_too_many - 1]);
			overboard = 1;
		}
	}
//	printf("OUT_OFF = %d at end (overboard? %d)\n", out_off, overboard);
*/
	leave_blocking_section();

//	printf("OB: %d, BO: %d, OO: %d\n", overboard, bit_off(&s), out_off);

	out_val = caml_alloc_tuple(3);
	Store_field(out_val, 0, Val_bool(overboard));
	Store_field(out_val, 1, Val_int(bit_off(&s)));
	Store_field(out_val, 2, Val_int(out_off));

	CAMLreturn(out_val);
}

CAMLprim value mfh_decode_count1_quants_bytecode(value *argv, int argn) {
	return mfh_decode_count1_quants(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}



static int decode_all_quants(
	seq_string_t *s,
	int16_t *out_quants,
	int in_off_too_many,
	int count0,
	int ht0,
	int count1,
	int ht1,
	int count2,
	int ht2,
	int out_off_too_many,
	int count1_ht1,
	int *error,
	int *overboard
) {

	int out_off = decode_all_big_quants(s, out_quants, in_off_too_many, count0, ht0, count1, ht1, count2, ht2, error);

	out_off = decode_count1_quants(s, out_quants, in_off_too_many, out_off, out_off_too_many, count1_ht1, overboard);

	return out_off;
}

CAMLprim value mfh_decode_all_quants(
	value in_ptr_val,
	value in_off_val,
	value out_ptr_val,
	value in_off_too_many_val,
	value count0_val,
	value ht0_val,
	value count1_val,
	value ht1_val,
	value count2_val,
	value ht2_val,
	value out_off_too_many_val,
	value count1_ht1_val
) {
	CAMLparam5(in_ptr_val, in_off_val, out_ptr_val, in_off_too_many_val, count0_val);
	CAMLxparam5(ht0_val, count1_val, ht1_val, count2_val, ht2_val);
	CAMLxparam2(out_off_too_many_val, count1_ht1_val);
	unsigned char *in_chars = (unsigned char *)Begin_val(in_ptr_val);
	int in_off = Int_val(in_off_val);
	int in_off_too_many = Int_val(in_off_too_many_val);
	int16_t *out_quants = (int16_t *)Begin_val(out_ptr_val);
	int count0 = Int_val(count0_val);
	int ht0 = Int_val(ht0_val);
	int count1 = Int_val(count1_val);
	int ht1 = Int_val(ht1_val);
	int count2 = Int_val(count2_val);
	int ht2 = Int_val(ht2_val);
	seq_string_t s;
	int out_off;
	int out_off_too_many = Int_val(out_off_too_many_val);
	int count1_ht1 = Int_val(count1_ht1_val);
	int overboard = 0;
	int error = 0;

	CAMLlocal1(out_val);

	enter_blocking_section();

	s.start_byte_ptr = in_chars;
	s.byte_ptr = in_chars + (in_off >> 3);
	s.bit_index = (in_off & 7);

	out_off = decode_all_quants(&s, out_quants, in_off_too_many, count0, ht0, count1, ht1, count2, ht2, out_off_too_many, count1_ht1, &error, &overboard);

//	out_off = decode_count1_quants(&s, out_quants, in_off_too_many, out_off, out_off_too_many, count1_ht1, &overboard);

	leave_blocking_section();

	out_val = caml_alloc_tuple(4);
	Store_field(out_val, 0, Val_bool(error));
	Store_field(out_val, 1, Val_bool(overboard));
	Store_field(out_val, 2, Val_int(bit_off(&s)));
	Store_field(out_val, 3, Val_int(out_off));

	CAMLreturn(out_val);
}

CAMLprim value mfh_decode_all_quants_bytecode(value *argv, int argn) {
	return mfh_decode_all_quants(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10], argv[11]);
}









/**********/
/* ENCODE */
/**********/


//short ht5[16/*x*/][16/*y*/][2] = {
//{
typedef struct {
	short bits;
	short val;
} encval;

encval enctab0[1] = {
	{0,0}
};
encval enctab1[4] = {
	{1,1},{3,1},
	{2,1},{3,0}
};
encval enctab2[9] = {
	{1,1},{3,2},{6,1},
	{3,3},{3,1},{5,1},
	{5,3},{5,2},{6,0}
};
encval enctab3[9] = {
	{2,3},{2,2},{6,1},
	{3,1},{2,1},{5,1},
	{5,3},{5,2},{6,0}
};
// No table 4
encval enctab5[16] = {
	{1,1},{3,2},{6,6},{7,5},
	{3,3},{3,1},{6,4},{7,4},
	{6,7},{6,5},{7,7},{8,1},
	{7,6},{6,1},{7,1},{8,0}
};
encval enctab6[16] = {
	{3,7},{3,3},{5,5},{7,1},
	{3,6},{2,2},{4,3},{5,2},
	{4,5},{4,4},{5,4},{6,1},
	{6,3},{5,3},{6,2},{7,0}
};
encval enctab7[36] = {
	{ 1, 1},{ 3, 2},{ 6,10},{ 8,19},{ 8,16},{ 9,10},
	{ 3, 3},{ 4, 3},{ 6, 7},{ 7,10},{ 7, 5},{ 8, 3},
	{ 6,11},{ 5, 4},{ 7,13},{ 8,17},{ 8, 8},{ 9, 4},
	{ 7,12},{ 7,11},{ 8,18},{ 9,15},{ 9,11},{ 9, 2},
	{ 7, 7},{ 7, 6},{ 8, 9},{ 9,14},{ 9, 3},{10, 1},
	{ 8, 6},{ 8, 4},{ 9, 5},{10, 3},{10, 2},{10, 0}
};
encval enctab8[36] = {
	{ 2, 3},{ 3, 4},{ 6, 6},{ 8,18},{ 8,12},{ 9, 5},
	{ 3, 5},{ 2, 1},{ 4, 2},{ 8,16},{ 8, 9},{ 8, 3},
	{ 6, 7},{ 4, 3},{ 6, 5},{ 8,14},{ 8, 7},{ 9, 3},
	{ 8,19},{ 8,17},{ 8,15},{ 9,13},{ 9,10},{10, 4},
	{ 8,13},{ 7, 5},{ 8, 8},{ 9,11},{10, 5},{10, 1},
	{ 9,12},{ 8, 4},{ 9, 4},{ 9, 1},{11, 1},{11, 0}
};
encval enctab9[36] = {
	{3, 7},{3, 5},{5, 9},{6,14},{8,15},{9, 7},
	{3, 6},{3, 4},{4, 5},{5, 5},{6, 6},{8, 7},
	{4, 7},{4, 6},{5, 8},{6, 8},{7, 8},{8, 5},
	{6,15},{5, 6},{6, 9},{7,10},{7, 5},{8, 1},
	{7,11},{6, 7},{7, 9},{7, 6},{8, 4},{9, 1},
	{8,14},{7, 4},{8, 6},{8, 2},{9, 6},{9, 0}
};
encval enctab10[64] = {
	{ 1, 1},{ 3, 2},{ 6,10},{ 8,23},{ 9,35},{ 9,30},{ 9,12},{10,17},
	{ 3, 3},{ 4, 3},{ 6, 8},{ 7,12},{ 8,18},{ 9,21},{ 8,12},{ 8, 7},
	{ 6,11},{ 6, 9},{ 7,15},{ 8,21},{ 9,32},{10,40},{ 9,19},{ 9, 6},
	{ 7,14},{ 7,13},{ 8,22},{ 9,34},{10,46},{10,23},{ 9,18},{10, 7},
	{ 8,20},{ 8,19},{ 9,33},{10,47},{10,27},{10,22},{10, 9},{10, 3},
	{ 9,31},{ 9,22},{10,41},{10,26},{11,21},{11,20},{10, 5},{11, 3},
	{ 8,14},{ 8,13},{ 9,10},{10,11},{10,16},{10, 6},{11, 5},{11, 1},
	{ 9, 9},{ 8, 8},{ 9, 7},{10, 8},{10, 4},{11, 4},{11, 2},{11, 0}
};
encval enctab11[64] = {
	{ 2, 3},{ 3, 4},{ 5,10},{ 7,24},{ 8,34},{ 9,33},{ 8,21},{ 9,15},
	{ 3, 5},{ 3, 3},{ 4, 4},{ 6,10},{ 8,32},{ 8,17},{ 7,11},{ 8,10},
	{ 5,11},{ 5, 7},{ 6,13},{ 7,18},{ 8,30},{ 9,31},{ 8,20},{ 8, 5},
	{ 7,25},{ 6,11},{ 7,19},{ 9,59},{ 8,27},{10,18},{ 8,12},{ 9, 5},
	{ 8,35},{ 8,33},{ 8,31},{ 9,58},{ 9,30},{10,16},{ 9, 7},{10, 5},
	{ 8,28},{ 8,26},{ 9,32},{10,19},{10,17},{11,15},{10, 8},{11,14},
	{ 8,14},{ 7,12},{ 7, 9},{ 8,13},{ 9,14},{10, 9},{10, 4},{10, 1},
	{ 8,11},{ 7, 4},{ 8, 6},{ 9, 6},{10, 6},{10, 3},{10, 2},{10, 0}
};
encval enctab12[64] = {
	{ 4, 9},{ 3, 6},{ 5,16},{ 7,33},{ 8,41},{ 9,39},{ 9,38},{ 9,26},
	{ 3, 7},{ 3, 5},{ 4, 6},{ 5, 9},{ 7,23},{ 7,16},{ 8,26},{ 8,11},
	{ 5,17},{ 4, 7},{ 5,11},{ 6,14},{ 7,21},{ 8,30},{ 7,10},{ 8, 7},
	{ 6,17},{ 5,10},{ 6,15},{ 6,12},{ 7,18},{ 8,28},{ 8,14},{ 8, 5},
	{ 7,32},{ 6,13},{ 7,22},{ 7,19},{ 8,18},{ 8,16},{ 8, 9},{ 9, 5},
	{ 8,40},{ 7,17},{ 8,31},{ 8,29},{ 8,17},{ 9,13},{ 8, 4},{ 9, 2},
	{ 8,27},{ 7,12},{ 7,11},{ 8,15},{ 8,10},{ 9, 7},{ 9, 4},{10, 1},
	{ 9,27},{ 8,12},{ 8, 8},{ 9,12},{ 9, 6},{ 9, 3},{ 9, 1},{10, 0}
};
encval enctab13[256] = {
	{ 1,  1},{ 4,  5},{ 6, 14},{ 7, 21},{ 8, 34},{ 9, 51},{ 9, 46},{10, 71},{ 9, 42},{10, 52},{11, 68},{11, 52},{12, 67},{12, 44},{13, 43},{13, 19},
	{ 3,  3},{ 4,  4},{ 6, 12},{ 7, 19},{ 8, 31},{ 8, 26},{ 9, 44},{ 9, 33},{ 9, 31},{ 9, 24},{10, 32},{10, 24},{11, 31},{12, 35},{12, 22},{12, 14},
	{ 6, 15},{ 6, 13},{ 7, 23},{ 8, 36},{ 9, 59},{ 9, 49},{10, 77},{10, 65},{ 9, 29},{10, 40},{10, 30},{11, 40},{11, 27},{12, 33},{13, 42},{13, 16},
	{ 7, 22},{ 7, 20},{ 8, 37},{ 9, 61},{ 9, 56},{10, 79},{10, 73},{10, 64},{10, 43},{11, 76},{11, 56},{11, 37},{11, 26},{12, 31},{13, 25},{13, 14},
	{ 8, 35},{ 7, 16},{ 9, 60},{ 9, 57},{10, 97},{10, 75},{11,114},{11, 91},{10, 54},{11, 73},{11, 55},{12, 41},{12, 48},{13, 53},{13, 23},{14, 24},
	{ 9, 58},{ 8, 27},{ 9, 50},{10, 96},{10, 76},{10, 70},{11, 93},{11, 84},{11, 77},{11, 58},{12, 79},{11, 29},{13, 74},{13, 49},{14, 41},{14, 17},
	{ 9, 47},{ 9, 45},{10, 78},{10, 74},{11,115},{11, 94},{11, 90},{11, 79},{11, 69},{12, 83},{12, 71},{12, 50},{13, 59},{13, 38},{14, 36},{14, 15},
	{10, 72},{ 9, 34},{10, 56},{11, 95},{11, 92},{11, 85},{12, 91},{12, 90},{12, 86},{12, 73},{13, 77},{13, 65},{13, 51},{14, 44},{16, 43},{16, 42},
	{ 9, 43},{ 8, 20},{ 9, 30},{10, 44},{10, 55},{11, 78},{11, 72},{12, 87},{12, 78},{12, 61},{12, 46},{13, 54},{13, 37},{14, 30},{15, 20},{15, 16},
	{10, 53},{ 9, 25},{10, 41},{10, 37},{11, 44},{11, 59},{11, 54},{13, 81},{12, 66},{13, 76},{13, 57},{14, 54},{14, 37},{14, 18},{16, 39},{15, 11},
	{10, 35},{10, 33},{10, 31},{11, 57},{11, 42},{12, 82},{12, 72},{13, 80},{12, 47},{13, 58},{14, 55},{13, 21},{14, 22},{15, 26},{16, 38},{17, 22},
	{11, 53},{10, 25},{10, 23},{11, 38},{12, 70},{12, 60},{12, 51},{12, 36},{13, 55},{13, 26},{13, 34},{14, 23},{15, 27},{15, 14},{15,  9},{16,  7},
	{11, 34},{11, 32},{11, 28},{12, 39},{12, 49},{13, 75},{12, 30},{13, 52},{14, 48},{14, 40},{15, 52},{15, 28},{15, 18},{16, 17},{16,  9},{16,  5},
	{12, 45},{11, 21},{12, 34},{13, 64},{13, 56},{13, 50},{14, 49},{14, 45},{14, 31},{14, 19},{14, 12},{15, 15},{16, 10},{15,  7},{16,  6},{16,  3},
	{13, 48},{12, 23},{12, 20},{13, 39},{13, 36},{13, 35},{15, 53},{14, 21},{14, 16},{17, 23},{15, 13},{15, 10},{15,  6},{17,  1},{16,  4},{16,  2},
	{12, 16},{12, 15},{13, 17},{14, 27},{14, 25},{14, 20},{15, 29},{14, 11},{15, 17},{15, 12},{16, 16},{16,  8},{19,  1},{18,  1},{19,  0},{16,  1}
};
// No table 14
encval enctab15[256] = {
	{ 3,  7},{ 4, 12},{ 5, 18},{ 7, 53},{ 7, 47},{ 8, 76},{ 9,124},{ 9,108},{ 9, 89},{10,123},{10,108},{11,119},{11,107},{11, 81},{12,122},{13, 63},
	{ 4, 13},{ 3,  5},{ 5, 16},{ 6, 27},{ 7, 46},{ 7, 36},{ 8, 61},{ 8, 51},{ 8, 42},{ 9, 70},{ 9, 52},{10, 83},{10, 65},{10, 41},{11, 59},{11, 36},
	{ 5, 19},{ 5, 17},{ 5, 15},{ 6, 24},{ 7, 41},{ 7, 34},{ 8, 59},{ 8, 48},{ 8, 40},{ 9, 64},{ 9, 50},{10, 78},{10, 62},{11, 80},{11, 56},{11, 33},
	{ 6, 29},{ 6, 28},{ 6, 25},{ 7, 43},{ 7, 39},{ 8, 63},{ 8, 55},{ 9, 93},{ 9, 76},{ 9, 59},{10, 93},{10, 72},{10, 54},{11, 75},{11, 50},{11, 29},
	{ 7, 52},{ 6, 22},{ 7, 42},{ 7, 40},{ 8, 67},{ 8, 57},{ 9, 95},{ 9, 79},{ 9, 72},{ 9, 57},{10, 89},{10, 69},{10, 49},{11, 66},{11, 46},{11, 27},
	{ 8, 77},{ 7, 37},{ 7, 35},{ 8, 66},{ 8, 58},{ 8, 52},{ 9, 91},{ 9, 74},{ 9, 62},{ 9, 48},{10, 79},{10, 63},{11, 90},{11, 62},{11, 40},{12, 38},
	{ 9,125},{ 7, 32},{ 8, 60},{ 8, 56},{ 8, 50},{ 9, 92},{ 9, 78},{ 9, 65},{ 9, 55},{10, 87},{10, 71},{10, 51},{11, 73},{11, 51},{12, 70},{12, 30},
	{ 9,109},{ 8, 53},{ 8, 49},{ 9, 94},{ 9, 88},{ 9, 75},{ 9, 66},{10,122},{10, 91},{10, 73},{10, 56},{10, 42},{11, 64},{11, 44},{11, 21},{12, 25},
	{ 9, 90},{ 8, 43},{ 8, 41},{ 9, 77},{ 9, 73},{ 9, 63},{ 9, 56},{10, 92},{10, 77},{10, 66},{10, 47},{11, 67},{11, 48},{12, 53},{12, 36},{12, 20},
	{ 9, 71},{ 8, 34},{ 9, 67},{ 9, 60},{ 9, 58},{ 9, 49},{10, 88},{10, 76},{10, 67},{11,106},{11, 71},{11, 54},{11, 38},{12, 39},{12, 23},{12, 15},
	{10,109},{ 9, 53},{ 9, 51},{ 9, 47},{10, 90},{10, 82},{10, 58},{10, 57},{10, 48},{11, 72},{11, 57},{11, 41},{11, 23},{12, 27},{13, 62},{12,  9},
	{10, 86},{ 9, 42},{ 9, 40},{ 9, 37},{10, 70},{10, 64},{10, 52},{10, 43},{11, 70},{11, 55},{11, 42},{11, 25},{12, 29},{12, 18},{12, 11},{13, 11},
	{11,118},{10, 68},{ 9, 30},{10, 55},{10, 50},{10, 46},{11, 74},{11, 65},{11, 49},{11, 39},{11, 24},{11, 16},{12, 22},{12, 13},{13, 14},{13,  7},
	{11, 91},{10, 44},{10, 39},{10, 38},{10, 34},{11, 63},{11, 52},{11, 45},{11, 31},{12, 52},{12, 28},{12, 19},{12, 14},{12,  8},{13,  9},{13,  3},
	{12,123},{11, 60},{11, 58},{11, 53},{11, 47},{11, 43},{11, 32},{11, 22},{12, 37},{12, 24},{12, 17},{12, 12},{13, 15},{13, 10},{12,  2},{13,  1},
	{12, 71},{11, 37},{11, 34},{11, 30},{11, 28},{11, 20},{11, 17},{12, 26},{12, 21},{12, 16},{12, 10},{12,  6},{13,  8},{13,  6},{13,  2},{13,  0}
};
encval enctab16[256] = {
	{ 1,   1},{ 4,   5},{ 6,  14},{ 8,  44},{ 9,  74},{ 9,  63},{10, 110},{10,  93},{11, 172},{11, 149},{11, 138},{12, 242},{12, 225},{12, 195},{13, 376},{ 9,  17},
	{ 3,   3},{ 4,   4},{ 6,  12},{ 7,  20},{ 8,  35},{ 9,  62},{ 9,  53},{ 9,  47},{10,  83},{10,  75},{10,  68},{11, 119},{12, 201},{11, 107},{12, 207},{ 8,   9},
	{ 6,  15},{ 6,  13},{ 7,  23},{ 8,  38},{ 9,  67},{ 9,  58},{10, 103},{10,  90},{11, 161},{10,  72},{11, 127},{11, 117},{11, 110},{12, 209},{12, 206},{ 9,  16},
	{ 8,  45},{ 7,  21},{ 8,  39},{ 9,  69},{ 9,  64},{10, 114},{10,  99},{10,  87},{11, 158},{11, 140},{12, 252},{12, 212},{12, 199},{13, 387},{13, 365},{10,  26},
	{ 9,  75},{ 8,  36},{ 9,  68},{ 9,  65},{10, 115},{10, 101},{11, 179},{11, 164},{11, 155},{12, 264},{12, 246},{12, 226},{13, 395},{13, 382},{13, 362},{ 9,   9},
	{ 9,  66},{ 8,  30},{ 9,  59},{ 9,  56},{10, 102},{11, 185},{11, 173},{12, 265},{11, 142},{12, 253},{12, 232},{13, 400},{13, 388},{13, 378},{14, 445},{10,  16},
	{10, 111},{ 9,  54},{ 9,  52},{10, 100},{11, 184},{11, 178},{11, 160},{11, 133},{12, 257},{12, 244},{12, 228},{12, 217},{13, 385},{13, 366},{14, 715},{10,  10},
	{10,  98},{ 9,  48},{10,  91},{10,  88},{11, 165},{11, 157},{11, 148},{12, 261},{12, 248},{13, 407},{13, 397},{13, 372},{13, 380},{15, 889},{15, 884},{10,   8},
	{10,  85},{10,  84},{10,  81},{11, 159},{11, 156},{11, 143},{12, 260},{12, 249},{13, 427},{13, 401},{13, 392},{13, 383},{14, 727},{14, 713},{14, 708},{10,   7},
	{11, 154},{10,  76},{10,  73},{11, 141},{11, 131},{12, 256},{12, 245},{13, 426},{13, 406},{13, 394},{13, 384},{14, 735},{13, 359},{14, 710},{13, 352},{11,  11},
	{11, 139},{11, 129},{10,  67},{11, 125},{12, 247},{12, 233},{12, 229},{12, 219},{13, 393},{14, 743},{14, 737},{14, 720},{15, 885},{15, 882},{14, 439},{10,   4},
	{12, 243},{11, 120},{11, 118},{11, 115},{12, 227},{12, 223},{13, 396},{14, 746},{14, 742},{14, 736},{14, 721},{14, 712},{14, 706},{13, 223},{14, 436},{11,   6},
	{12, 202},{12, 224},{12, 222},{12, 218},{12, 216},{13, 389},{13, 386},{13, 381},{13, 364},{15, 888},{14, 443},{14, 707},{14, 440},{14, 437},{16,1728},{11,   4},
	{14, 747},{12, 211},{12, 210},{12, 208},{13, 370},{13, 379},{14, 734},{14, 723},{14, 714},{16,1735},{15, 883},{15, 877},{15, 876},{17,3459},{15, 865},{11,   2},
	{13, 377},{13, 369},{11, 102},{12, 187},{14, 726},{14, 722},{13, 358},{14, 711},{14, 709},{15, 866},{16,1734},{15, 871},{17,3458},{15, 870},{14, 434},{11,   0},
	{ 9,  12},{ 8,  10},{ 8,   7},{ 9,  11},{ 9,  10},{10,  17},{10,  11},{10,   9},{11,  13},{11,  12},{11,  10},{11,   7},{11,   5},{11,   3},{11,   1},{ 8,   3}
};
// 17-23 are the same as 16
encval enctab24[256] = {
	{ 4,  15},{ 4,  13},{ 6,  46},{ 7,  80},{ 8, 146},{ 9, 262},{ 9, 248},{10, 434},{10, 426},{11, 669},{11, 653},{11, 649},{11, 621},{11, 517},{12,1032},{ 9,  88},
	{ 4,  14},{ 4,  12},{ 5,  21},{ 6,  38},{ 7,  71},{ 8, 130},{ 8, 122},{ 9, 216},{ 9, 209},{ 9, 198},{10, 327},{10, 345},{10, 319},{10, 297},{10, 279},{ 8,  42},
	{ 6,  47},{ 5,  22},{ 6,  41},{ 7,  74},{ 7,  68},{ 8, 128},{ 8, 120},{ 9, 221},{ 9, 207},{ 9, 194},{ 9, 182},{10, 340},{10, 315},{10, 295},{11, 541},{ 7,  18},
	{ 7,  81},{ 6,  39},{ 7,  75},{ 7,  70},{ 8, 134},{ 8, 125},{ 8, 116},{ 9, 220},{ 9, 204},{ 9, 190},{ 9, 178},{10, 325},{10, 311},{10, 293},{10, 271},{ 7,  16},
	{ 8, 147},{ 7,  72},{ 7,  69},{ 8, 135},{ 8, 127},{ 8, 118},{ 8, 112},{ 9, 210},{ 9, 200},{ 9, 188},{10, 352},{10, 323},{10, 306},{10, 285},{11, 540},{ 7,  14},
	{ 9, 263},{ 7,  66},{ 8, 129},{ 8, 126},{ 8, 119},{ 8, 114},{ 9, 214},{ 9, 202},{ 9, 192},{ 9, 180},{10, 341},{10, 317},{10, 301},{10, 281},{10, 262},{ 7,  12},
	{ 9, 249},{ 8, 123},{ 8, 121},{ 8, 117},{ 8, 113},{ 9, 215},{ 9, 206},{ 9, 195},{ 9, 185},{10, 347},{10, 330},{10, 308},{10, 291},{10, 272},{11, 520},{ 7,  10},
	{10, 435},{ 8, 115},{ 8, 111},{ 8, 109},{ 9, 211},{ 9, 203},{ 9, 196},{ 9, 187},{10, 353},{10, 332},{10, 313},{10, 298},{10, 283},{11, 531},{11, 381},{ 8,  17},
	{10, 427},{ 9, 212},{ 9, 208},{ 9, 205},{ 9, 201},{ 9, 193},{ 9, 186},{ 9, 177},{ 9, 169},{10, 320},{10, 303},{10, 286},{10, 268},{11, 514},{11, 377},{ 8,  16},
	{10, 335},{ 9, 199},{ 9, 197},{ 9, 191},{ 9, 189},{ 9, 181},{ 9, 174},{10, 333},{10, 321},{10, 305},{10, 289},{10, 275},{11, 521},{11, 379},{11, 371},{ 8,  11},
	{11, 668},{ 9, 184},{ 9, 183},{ 9, 179},{ 9, 175},{10, 344},{10, 331},{10, 314},{10, 304},{10, 290},{10, 277},{11, 530},{11, 383},{11, 373},{11, 366},{ 8,  10},
	{11, 652},{10, 346},{ 9, 171},{ 9, 168},{ 9, 164},{10, 318},{10, 309},{10, 299},{10, 287},{10, 276},{10, 263},{11, 513},{11, 375},{11, 368},{11, 362},{ 8,   6},
	{11, 648},{10, 322},{10, 316},{10, 312},{10, 307},{10, 302},{10, 292},{10, 284},{10, 269},{10, 261},{11, 512},{11, 376},{11, 370},{11, 364},{11, 359},{ 8,   4},
	{11, 620},{10, 300},{10, 296},{10, 294},{10, 288},{10, 282},{10, 273},{10, 266},{11, 515},{11, 380},{11, 374},{11, 369},{11, 365},{11, 361},{11, 357},{ 8,   2},
	{12,1033},{10, 280},{10, 278},{10, 274},{10, 267},{10, 264},{10, 259},{11, 382},{11, 378},{11, 372},{11, 367},{11, 363},{11, 360},{11, 358},{11, 356},{ 8,   0},
	{ 8,  43},{ 7,  20},{ 7,  19},{ 7,  17},{ 7,  15},{ 7,  13},{ 7,  11},{ 7,   9},{ 7,   7},{ 7,   6},{ 7,   4},{ 8,   7},{ 8,   5},{ 8,   3},{ 8,   1},{ 4,   3}
};
// 25-31 are the same as 24

encval enctab1_a[16] = {
	{1, 1}, {4, 5}, {4, 4}, {5, 5},
	{4, 6}, {6, 5}, {5, 4}, {6, 4},
	{4, 7}, {5, 3}, {5, 6}, {6, 0},
	{5, 7}, {6, 2}, {6, 3}, {6, 1}
};

// I don't think I really need this since it's just {4, (15 ^ index)}
/*
encval enctab1_b[16] = {
	{4,15}, {4,14}, {4,13}, {4,12},
	{4,11}, {4,10}, {4, 9}, {4, 8},
	{4, 7}, {4, 6}, {4, 5}, {4, 4},
	{4, 3}, {4, 2}, {4, 1}, {4, 0}
};
*/

typedef struct {
	encval *tab;
	char multiplier;
} enctab;

enctab enctabs[] = {
	{enctab0,   1},
	{enctab1,   2},
	{enctab2,   3},
	{enctab3,   3},
	{NULL,      1},
	{enctab5,   4},
	{enctab6,   4},
	{enctab7,   6},
	{enctab8,   6},
	{enctab9,   6},
	{enctab10,  8},
	{enctab11,  8},
	{enctab12,  8},
	{enctab13, 16},
	{NULL,      1},
	{enctab15, 16},
	{enctab16, 16},
	{enctab16, 16},
	{enctab16, 16},
	{enctab16, 16},
	{enctab16, 16},
	{enctab16, 16},
	{enctab16, 16},
	{enctab16, 16},
	{enctab24, 16},
	{enctab24, 16},
	{enctab24, 16},
	{enctab24, 16},
	{enctab24, 16},
	{enctab24, 16},
	{enctab24, 16},
	{enctab24, 16}
};



// XXX PTR_SEQ XXX //
// Maybe this should go somewhere in Ptr?
typedef struct {
	unsigned char *seq_ptr;
	uintnat seq_val;
	unsigned int seq_bits;
	unsigned char *seq_orig_ptr;
} seq_t;

seq_t make_seq(unsigned char *ptr) {
	seq_t out;
	out.seq_ptr = ptr;
	out.seq_val = 0;
	out.seq_bits = 0;
	out.seq_orig_ptr = ptr;
	return(out);
}
seq_t continue_seq(unsigned char *ptr, int bits) {
	seq_t out = make_seq(ptr);
	out.seq_ptr += bits >> 3;
	if(bits & 7) {
		// Have to get the current byte here
		out.seq_bits = (bits & 7);
		out.seq_val = (out.seq_ptr[0] >> (8 - out.seq_bits));
	}
	return(out);
}

void write_seq(seq_t *sp) {
	while(sp->seq_bits >= 8) {
		unsigned int shift_amount = sp->seq_bits - 8;
		unsigned char write_val = sp->seq_val >> shift_amount;
		sp->seq_ptr[0] = write_val;
		sp->seq_ptr++;
		sp->seq_val ^= write_val << shift_amount;
		sp->seq_bits -= 8;
	}
}
// This does NOT update seq_ptr with the final byte
// so that we can continue with write_seq in the future
int finalize_seq(seq_t *sp) {
	if(sp->seq_bits > 0) {
		write_seq(sp);
		sp->seq_ptr[0] = sp->seq_val << (8 - sp->seq_bits);
	}
	return(8 * (sp->seq_ptr - sp->seq_orig_ptr) + sp->seq_bits);
}

void add_seq(seq_t *sp, int bits, uintnat val) {
	sp->seq_val = (sp->seq_val << bits) | val;
	sp->seq_bits += bits;
}
// XXX !PTR_SEQ XXX //


static encval simple_lookup(unsigned int t, unsigned int x, unsigned int y) {
	enctab tab = enctabs[t];
	return(tab.tab[x * tab.multiplier + y]);
}

/*
CAMLprim value simple_lookup(value tv, value xv, value yv) {
	CAMLparam3(tv,xv,yv);
	int t = Int_val(tv);
	int x = Int_val(xv);
	int y = Int_val(yv);
	encval v;
	CAMLlocal1(out_val);

	v = simple_lookup_internal(t,x,y);

	out_val = caml_alloc_tuple(2);
	Store_field(out_val, 0, Val_int(v.bits));
	Store_field(out_val, 1, Val_int(v.val));

	CAMLreturn(out_val);
}
*/
static void write_big_quants(unsigned int t, int x, int y, seq_t *sp) {
	unsigned int x_abs = abs(x);
	unsigned int y_abs = abs(y);
	unsigned int x_max_15 = (x_abs > 15 ? 15 : x_abs);
	unsigned int y_max_15 = (y_abs > 15 ? 15 : y_abs);
	int x_sign = (x < 0 ? 1 : 0);
	int y_sign = (y < 0 ? 1 : 0);
	int x_sign_bits = (x == 0 ? 0 : 1);
	int y_sign_bits = (y == 0 ? 0 : 1);
	int linbits = linbits_table[t];
	int x_linbits = (x_abs >= 15 ? linbits : 0);
	int y_linbits = (y_abs >= 15 ? linbits : 0);
	int x_lin = x_abs - x_max_15;
	int y_lin = y_abs - y_max_15;
	encval put;

//	printf("Getting %d,%d from %d\n", x_max_15, y_max_15, t);
//	fflush(stdout);

	put = simple_lookup(t, x_max_15, y_max_15);

//	printf("Got %d with %d bits\n", put.val, put.bits);
//	fflush(stdout);

	add_seq(sp, put.bits, put.val);
	write_seq(sp);
	add_seq(sp, x_linbits, x_lin);
	add_seq(sp, x_sign_bits, x_sign);
	write_seq(sp);
	add_seq(sp, y_linbits, y_lin);
	add_seq(sp, y_sign_bits, y_sign);
	write_seq(sp);
}
/*
CAMLprim value write_big_quants_test(value tv, value xv, value yv, value ptr_val, value bit_off_val) {
	CAMLparam5(tv,xv,yv,ptr_val,bit_off_val);
	unsigned char *ptr = Begin_val(ptr_val);
	seq_t s = continue_seq(ptr, Int_val(bit_off_val));
	unsigned int t = Int_val(tv);
	int x = Int_val(xv);
	int y = Int_val(yv);

	write_big_quants(t,x,y,&s);

	CAMLreturn(Val_int(finalize_seq(&s)));
}
*/
static void write_big_to_frame(int16_t *quants, int index, int left, int hti, seq_t *sp) {
	int16_t x,y;
	while(left > 0) {
		x = quants[index++];
		y = quants[index++];

//		printf(" %d %d", x, y);

		write_big_quants(hti, x, y, sp);

		left--;
	}
}

CAMLprim value mfh_write_big_to_frame(value quants_val, value index_val, value left_val, value hti_val, value ptr_val, value bit_off_val) {
	CAMLparam5(quants_val, index_val, left_val, hti_val, ptr_val);
	CAMLxparam1(bit_off_val);
	int16_t *quants = (int16_t *)Begin_val(quants_val);
	int index = Int_val(index_val);
	int left = Int_val(left_val);
	int hti = Int_val(hti_val);
	unsigned char *ptr = (unsigned char *)Begin_val(ptr_val);
	int bit_off = Int_val(bit_off_val);

	seq_t s = continue_seq(ptr, bit_off);

	enter_blocking_section();
	write_big_to_frame(quants, index, left, hti, &s);
	leave_blocking_section();

	CAMLreturn(Val_int(finalize_seq(&s)));
}

CAMLprim value mfh_write_big_to_frame_bytecode(value *argv, value argn) {
	return(mfh_write_big_to_frame(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]));
}

static void write_part1_quants(int tb, int v, int w, int x, int y, seq_t *sp) {
	unsigned int index = (((v & 1) << 3) | ((w & 1) << 2) | ((x & 1) << 1) | (y & 1));
	if(tb) {
//		encval put = enctab1_b[index];
//		add_seq(sp, put.bits, put.val);
		add_seq(sp, 4, 0xF ^ index);
	} else {
		encval put = enctab1_a[index];
		add_seq(sp, put.bits, put.val);
	}
	if(index & 8) {
		add_seq(sp, 1, ((v >> 1) & 1));
	}
	if(index & 4) {
		add_seq(sp, 1, ((w >> 1) & 1));
	}
	if(index & 2) {
		add_seq(sp, 1, ((x >> 1) & 1));
	}
	if(index & 1) {
		add_seq(sp, 1, ((y >> 1) & 1));
	}
	write_seq(sp);
//	printf("%d%d%d%d%d\n", index, v_sign, w_sign, x_sign, y_sign);
}

static void write_part1_to_frame(int16_t *quants, int index, int index_to, int part1_table_b, seq_t *sp) {
	int16_t v,w,x,y;
	while(index <= index_to) {
		v = quants[index++];
		w = quants[index++];
		x = quants[index++];
		y = quants[index++];

//		printf(" %d %d %d %d", v, w, x, y);

//		printf(" %d", (sp->seq_ptr - sp->seq_orig_ptr) * 8 + sp->seq_bits);
		write_part1_quants(part1_table_b, v, w, x, y, sp);
//		printf("->%d", (sp->seq_ptr - sp->seq_orig_ptr) * 8 + sp->seq_bits);
	}
}

static int find_last_nonzero_index(int16_t *ptr, int start_i) {
	int i;
	for(i = start_i; i >= 0 && ptr[i] == 0; i--) {}
	return(i);
}
CAMLprim value flni_test(value ptr_val) {
	CAMLparam1(ptr_val);
	int16_t *ptr = (int16_t *)Begin_val(ptr_val);
	int start_i = Length_val(ptr_val) / 2 - 1;
	CAMLreturn(Val_int(find_last_nonzero_index(ptr, start_i)));
}

static void write_all_to_frame(
	int16_t *quants,
	int num_quants,
	int region0_count,
	int region0_table,
	int region1_count,
	int region1_table,
	int region2_count,
	int region2_table,
	int part1_table_b,
	seq_t *sp
) {
	int last_nonzero_index;
	write_big_to_frame(quants, 0, region0_count, region0_table, sp);
	write_big_to_frame(quants, region0_count * 2, region1_count, region1_table, sp);
	write_big_to_frame(quants, (region0_count + region1_count) * 2, region2_count, region2_table, sp);

	// part1
//	printf("*");
	last_nonzero_index = find_last_nonzero_index(quants, num_quants - 1);
	write_part1_to_frame(quants, (region0_count + region1_count + region2_count) * 2, last_nonzero_index, part1_table_b, sp);
}

CAMLprim value mfh_write_all_to_frame(
	value quants_val,
	value region0_count_val,
	value region0_table_val,
	value region1_count_val,
	value region1_table_val,
	value region2_count_val,
	value region2_table_val,
	value part1_table_b_val,
	value ptr_val,
	value bit_off_val
) {
	CAMLparam5(quants_val, region0_count_val, region0_table_val, region1_count_val, region1_table_val);
	CAMLxparam5(region2_count_val, region2_table_val, ptr_val, part1_table_b_val, bit_off_val);

	int16_t *quants = (int16_t *)Begin_val(quants_val);
	int num_quants = Length_val(quants_val) / 2;
	int region0_count = Int_val(region0_count_val);
	int region0_table = Int_val(region0_table_val);
	int region1_count = Int_val(region1_count_val);
	int region1_table = Int_val(region1_table_val);
	int region2_count = Int_val(region2_count_val);
	int region2_table = Int_val(region2_table_val);
	int part1_table_a = Bool_val(part1_table_b_val);
	unsigned char *ptr = (unsigned char *)Begin_val(ptr_val);
	int bit_off = Int_val(bit_off_val);

	seq_t s = continue_seq(ptr, bit_off);

//	fflush(stdout);
//	printf("QUANTS IN C:");

	enter_blocking_section();
	write_all_to_frame(quants, num_quants, region0_count, region0_table, region1_count, region1_table, region2_count, region2_table, part1_table_a, &s);
	leave_blocking_section();

//	printf("\n");
//	fflush(stdout);

	CAMLreturn(Val_int(finalize_seq(&s)));
}

CAMLprim value mfh_write_all_to_frame_bytecode(value *argv, value argn) {
	return(mfh_write_all_to_frame(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9]));
}

