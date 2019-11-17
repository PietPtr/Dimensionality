#ifndef INIT_H
#define INIT_H

#include <SDL.h>

void init();
int events(SDL_Event* event);
void loop(SDL_Renderer* renderer, double dt, int frame);
void drawAt(SDL_Renderer* renderer, SDL_Texture* texture, int x, int y);

#endif
