
#include "loaders.h"
#include <stdbool.h>

SDL_Renderer* renderer;

SDL_Texture* loadBMP(char* name)
{
    SDL_Surface* surface = SDL_LoadBMP( name );
    if( surface == NULL )
    {
        printf( "Unable to load image %s! SDL Error: %s\n", name, SDL_GetError() );
        return NULL;
    }

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    printf("loading %s\n", name);

    return texture;
}
