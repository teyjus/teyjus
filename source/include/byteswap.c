#include "byteswap.h"

u_int16_t bswap_16(u_int16_t x)
{
  return ((x >> 8) | (x << 8));
}

u_int32_t bswap_32(u_int32_t x)
{
  return (bswap_16(x & 0xffff) << 16) | bswap_16(x >> 16);
}