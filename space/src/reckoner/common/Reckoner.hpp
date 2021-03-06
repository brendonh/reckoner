#pragma once
#ifndef __RECKONER_MISC
#define __RECKONER_MISC

// Suppress unused parameter warnings
#ifdef UNUSED
#elif defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif

#define PI 3.1415927
#define TWOPI (2*PI)
#define DEG2RAD (PI / 180.f)
#define RAD2DEG (180.f / PI)

#define MESSAGE_TYPE_PREFIX_LENGTH 2

#define TIMESTEP (1000.0f / 60.0f)

#endif
