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
int dimension = 1;

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
    drawNavigator(renderer);

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

#define NAVBOX_SIZE 32
#define LINE_WHITE 255
#define NAV_LENGTH 256
#define NAV_SQUARE_SIZE 64
void drawNavigator(SDL_Renderer* renderer)
{
    SDL_Rect center = {
        navigatorContainer.x + navigatorContainer.w / 2,
        navigatorContainer.y + navigatorContainer.h / 2
    };


    double angle = M_PI / dimension;

    for (int d = 0; d < dimension; d++) {
        SDL_SetRenderDrawColor(renderer, LINE_WHITE, LINE_WHITE, LINE_WHITE, 255);
        int dirx = (int)(cos(angle * d + 0.5 * M_PI) * NAV_LENGTH);
        int diry = (int)(sin(angle * d + 0.5 * M_PI) * NAV_LENGTH);

        SDL_RenderDrawLine(renderer,
            center.x - dirx,
            center.y - diry,
            center.x + dirx,
            center.y + diry
        );

        SDL_RenderFillRect(renderer, &((SDL_Rect) {
            center.x - dirx - NAV_SQUARE_SIZE / 2,
            center.y - diry - NAV_SQUARE_SIZE / 2,
            NAV_SQUARE_SIZE,
            NAV_SQUARE_SIZE
        }));

        SDL_RenderFillRect(renderer, &((SDL_Rect) {
            center.x + dirx - NAV_SQUARE_SIZE / 2,
            center.y + diry - NAV_SQUARE_SIZE / 2,
            NAV_SQUARE_SIZE,
            NAV_SQUARE_SIZE
        }));

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);

        SDL_RenderFillRect(renderer, &((SDL_Rect) {
            center.x - dirx - NAV_SQUARE_SIZE / 2 + 1,
            center.y - diry - NAV_SQUARE_SIZE / 2 + 1,
            NAV_SQUARE_SIZE - 2,
            NAV_SQUARE_SIZE - 2
        }));

        SDL_RenderFillRect(renderer, &((SDL_Rect) {
            center.x + dirx - NAV_SQUARE_SIZE / 2 + 1,
            center.y + diry - NAV_SQUARE_SIZE / 2 + 1,
            NAV_SQUARE_SIZE - 2,
            NAV_SQUARE_SIZE - 2
        }));
    }
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
