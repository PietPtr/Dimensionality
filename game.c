#ifdef __GLASGOW_HASKELL__
#include "DAWG_stub.h"
#endif

#include "game.h"
#include "loaders.h"
#include "consts.h"
#include <stdbool.h>
#include <math.h>

SDL_Surface* screen = NULL;
SDL_Texture* tCharacter = NULL;

double totalTime = 0;

void init()
{
    tCharacter = loadBMP("resources/player.bmp");
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

    drawMenu(renderer);

    if ( (frame - 100) % 5000 == 0 ) {
        printf("%f\n", 1 / dt);
    }
}

#define PADDING 5
#define MENU_BG_GRAY 100
#define MENU_FG_GRAY 200
// Global becasue other stuff wants to render relative to these
SDL_Rect navigatorContainer;
SDL_Rect menuContainer;

void drawMenu(SDL_Renderer* renderer)
{
    SDL_SetRenderDrawColor(renderer, MENU_BG_GRAY, MENU_BG_GRAY, MENU_BG_GRAY, 255);

    navigatorContainer = (SDL_Rect) {
        .x = PADDING,
        .y = PADDING,
        .w = SCREEN_HEIGHT - PADDING,
        .h = SCREEN_HEIGHT - 2 * PADDING };
    menuContainer = (SDL_Rect) {
        .x = navigatorContainer.x + navigatorContainer.w + PADDING,
        .y = PADDING,
        .w = SCREEN_WIDTH - navigatorContainer.w - 3 * PADDING,
        .h = SCREEN_HEIGHT - 2 * PADDING };

    SDL_RenderDrawRect(renderer, &navigatorContainer);
    SDL_RenderDrawRect(renderer, &menuContainer);
}

void drawNavigator(SDL_Renderer* renderer)
{

}

void drawCharacter(SDL_Renderer* renderer)
{

}

void drawAt(SDL_Renderer* renderer, SDL_Texture* texture, int x, int y)
{
    int width, height;
    SDL_QueryTexture(texture, NULL, NULL, &width, &height);

    SDL_Rect dstrect;
	dstrect.x = x;
	dstrect.y = y;
    dstrect.w = width;
    dstrect.h = height;

    SDL_RenderCopy(renderer, texture, NULL, &dstrect);
}
