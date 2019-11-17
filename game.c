#ifdef __GLASGOW_HASKELL__
#include "DAWG_stub.h"
#endif

#include "game.h"
#include "loaders.h"
#include "consts.h"
#include <stdbool.h>
#include <math.h>

SDL_Surface* screen = NULL;
SDL_Texture* tHello = NULL;

double totalTime = 0;

void init()
{
    tHello = loadBMP("resources/hello_world.bmp");
}

int events(SDL_Event* event)
{
    if ( event->window.type == SDL_QUIT ) {
        return CODE_QUIT;
    } else {
        return CODE_NONE;
    }
}

void loop(SDL_Renderer* renderer, double dt, int frame)
{
    totalTime += dt;

    drawAt(renderer, tHello, (int)(sin((double)totalTime * 4) * 100), 0);

    printf("%i\n", query_dawg(0, frame / 5000, 0));

    if ( frame % 5000 == 0 ) {
        printf("%f\n", 1 / dt);
    }
}

void drawAt(SDL_Renderer* renderer, SDL_Texture* texture, int x, int y) {
    int width, height;
    SDL_QueryTexture(texture, NULL, NULL, &width, &height);

    SDL_Rect dstrect;
	dstrect.x = x;
	dstrect.y = y;
    dstrect.w = width;
    dstrect.h = height;

    SDL_RenderCopy(renderer, tHello, NULL, &dstrect);
}
