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
SDL_Texture* tFont = NULL;
SDL_Texture* tDimensions = NULL;
SDL_Texture* tTiles = NULL;

double totalTime = 0;
int dimension = 3;
int dimensionColors[] = {
    0xff0000, 0x00ff00, 0x3333ff, 0x00ffff,
    0xff00ff, 0xffff00, 0xff8000, 0x0080ff,
    0x8080ff, 0xff0080, 0x80ff80, 0x8000ff };
int position[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
struct Axis selectedAxis = { -1, 1 };

void init()
{
    tCharacter = loadBMP("resources/player.bmp");
    tFont = loadBMP("resources/font.bmp");
    tDimensions = loadBMP("resources/dimensions.bmp");
    tTiles = loadBMP("resources/tiles.bmp");
}

int events(SDL_Event* event)
{
    switch (event->window.type) {
        case SDL_QUIT:
            return CODE_QUIT;
        case SDL_MOUSEMOTION:
            handleMouseMove(event->motion.x, event->motion.y,
                event->motion.xrel, event->motion.yrel);
            return CODE_NONE;
        case SDL_MOUSEBUTTONUP:
            printf("%i\n", event->button.timestamp);
            handleClick(event->button.button, event->button.x, event->button.y);
            return CODE_NONE;
    }
    return CODE_NONE;
}

void loop(SDL_Renderer* renderer, double dt, int frame)
{
    totalTime += dt;

    drawMenu(renderer);
    drawNavigator(renderer);

    drawText(renderer, "Dimensional Navigator", 10, 10);
    drawDimensions(renderer, position, 10, 25);

    if ( (frame - 100) % 5000 == 0 ) {
        // printf("%f\n", 1 / dt);
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
#define TILE_SIZE 64
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

        int color = dimensionColors[d];
        if (selectedAxis.axis == d && selectedAxis.direction == -1) {
            SDL_SetRenderDrawColor(renderer, r(color), g(color), b(color), 255);
        }
        SDL_RenderDrawLine(renderer, center.x, center.y, center.x + dirx, center.y + diry);
        SDL_SetRenderDrawColor(renderer, LINE_WHITE, LINE_WHITE, LINE_WHITE, 255);

        if (selectedAxis.axis == d && selectedAxis.direction == 1) {
            SDL_SetRenderDrawColor(renderer, r(color), g(color), b(color), 255);
        }

        SDL_RenderDrawLine(renderer, center.x, center.y, center.x - dirx, center.y - diry);
        SDL_SetRenderDrawColor(renderer, LINE_WHITE, LINE_WHITE, LINE_WHITE, 255);


        SDL_Rect upperSquare = {
            center.x - dirx - NAV_SQUARE_SIZE / 2,
            center.y - diry - NAV_SQUARE_SIZE / 2,
            NAV_SQUARE_SIZE,
            NAV_SQUARE_SIZE
        };

        SDL_RenderFillRect(renderer, &upperSquare);

        SDL_Rect lowerSquare = {
            center.x + dirx - NAV_SQUARE_SIZE / 2,
            center.y + diry - NAV_SQUARE_SIZE / 2,
            NAV_SQUARE_SIZE,
            NAV_SQUARE_SIZE
        };

        SDL_RenderFillRect(renderer, &lowerSquare);

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

        for (int dir = -1; dir <= 1; dir += 2) {
            int tempPos[12];
            memcpy(tempPos, position, sizeof(tempPos));
            tempPos[d] += dir;

            int tile = query_dawg(tempPos);

            SDL_Rect srcRect = {
                (tile % 4) * TILE_SIZE,
                (tile / 4) * TILE_SIZE,
                TILE_SIZE,
                TILE_SIZE
            };

            SDL_Rect* square = dir == -1 ? &lowerSquare : &upperSquare;

            SDL_Rect dstRect = {
                square->x,
                square->y,
                TILE_SIZE,
                TILE_SIZE
            };

            SDL_RenderCopy(renderer, tTiles, &srcRect, &dstRect);
        }


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

#define CHAR_WIDTH 9
#define CHAR_HEIGHT 15
void drawText(SDL_Renderer* renderer, char text[], int x, int y)
{
    int i = 0;
    char character = text[0];
    while (character != 0) {
        int index = (int)character - 32;
        SDL_Rect srcRect = {
            CHAR_WIDTH * (index % 16),
            CHAR_HEIGHT * (index / 16),
            CHAR_WIDTH,
            CHAR_HEIGHT
        };

        SDL_Rect dstRect = {
            x + i * CHAR_WIDTH,
            y,
            CHAR_WIDTH,
            CHAR_HEIGHT
        };

        SDL_RenderCopy(renderer, tFont, &srcRect, &dstRect);

        i++;
        character = text[i];
    }
}

#define DIMCHAR_W 10
#define DIMCHAR_H 20
void drawDimensions(SDL_Renderer* renderer, int position[], int x, int y) {
    int offset = 0;
    for (int d = 0; d < dimension; d++) {
        SDL_Rect srcRect = {
            DIMCHAR_W * d,
            0,
            DIMCHAR_W,
            DIMCHAR_H
        };

        SDL_Rect dstRect = {
            x + offset,
            y,
            DIMCHAR_W,
            DIMCHAR_H
        };

        int color = dimensionColors[d];
        SDL_SetTextureColorMod(tDimensions, r(color), g(color), b(color));
        SDL_RenderCopy(renderer, tDimensions, &srcRect, &dstRect);

        offset += DIMCHAR_W;

        drawText(renderer, ":", x + offset, y + 4);
        offset += CHAR_WIDTH;

        char buffer[12];
        sprintf(buffer, "%d", position[d]);
        drawText(renderer, buffer, x + offset, y + 4);
        offset += strlen(buffer) * CHAR_WIDTH + 3;

    }
}

Uint8 r(int color) {
    return color >> 16 & 0xff;
}

Uint8 g(int color) {
    return color >> 8 & 0xff;
}

Uint8 b(int color) {
    return color & 0xff;
}

void handleMouseMove(int x, int y, int xrel, int yrel) {
    SDL_Rect center = {
        navigatorContainer.x + navigatorContainer.w / 2,
        navigatorContainer.y + navigatorContainer.h / 2
    };

    if (x > SCREEN_HEIGHT) {
        selectedAxis.axis = -1;
        return;
    }

    double angle = atan((double)(center.y - y) / (double)(center.x - x)) + 0.5 * M_PI;
    angle = x <= center.x ? angle + M_PI : angle;

    double arc = M_PI / dimension;
    double offset = M_PI / (2 * dimension);
    for (int d = 0; d < dimension * 2; d++) {
        double lineAngle = d * arc;

        double rangeStart = lineAngle - offset;
        rangeStart = rangeStart < 0 ? rangeStart + 2 * M_PI : rangeStart;

        double shiftEnd = lineAngle + offset - rangeStart;
        shiftEnd = shiftEnd < 0 ? shiftEnd + 2 * M_PI : shiftEnd;
        double shiftAngle = angle - rangeStart;
        shiftAngle = shiftAngle < 0 ? shiftAngle + 2 * M_PI : shiftAngle;

        if (shiftAngle < shiftEnd) {
            selectedAxis.axis = d % dimension;
            selectedAxis.direction = d / dimension == 0 ? 1 : -1;
        }
    }
}

void handleClick(int button, int x, int y) {
    if (selectedAxis.axis > -1) {
        position[selectedAxis.axis] += selectedAxis.direction;
    }
}
