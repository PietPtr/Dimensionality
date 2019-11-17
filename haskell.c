#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "DAWG_stub.h"
#endif
#include <stdio.h>
#include <SDL.h>

int main(int argc, char *argv[])
{
    int i;
    hs_init(&argc, &argv);

    for (int x = -40; x <= 40; x++) {
        for (int y = -40; y <= 40; y++) {
            for (int z = -40; z <= 40; z++) {
                int tile = query_dawg(0, 0, 0);
                printf("[[%i,%i,%i], %i],", x, y, z, tile);
            }
        }
    }

    hs_exit();
    return 0;
}
