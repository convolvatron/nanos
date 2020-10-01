/*
   SHA-1 in C
   By Steve Reid <steve@edmweb.com>
   100% Public Domain
*/

typedef struct
{
    u32 state[5];
    u32 count[2];
    unsigned char buffer[64];
} SHA1_CTX;

void SHA1Transform(
                   u32 state[5],
                       const unsigned char buffer[64]
                   );

void SHA1Init(
                  SHA1_CTX * context
              );

void SHA1Update(
                SHA1_CTX * context,
                const unsigned char *data,
                    u32 len
                );

void SHA1Final(
               unsigned char digest[20],
                   SHA1_CTX * context
               );

void sha1(buffer out, buffer in);



