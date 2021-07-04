﻿
#include <stdio.h>
#include <iostream>
#include "cuda_runtime.h"
#include <device_launch_parameters.h>
#include <stdlib.h>
using namespace std;

//variables globales
#define num_filas_RGB 32           // num filas matriz RGB
#define num_columnas_RGB 32        // num columnas matriz RGB
#define width_RGB 32               // width matriz RGB
#define num_filas_filtro 3         // num filas matriz Filtro
#define num_columnas_filtro 3      // num columnas matriz Filtro
#define width_filtro 3             // width matriz Filtro
#define tile_width 2

// Declaración de funciones
void crearMatriz(int matriz[num_filas_RGB][num_columnas_RGB]);
void imprimir_matriz(int a[num_filas_RGB][num_columnas_RGB]);
void imprimir_matriz_filtro(int a[num_filas_filtro][num_columnas_filtro]);


// Suma matrices
__global__ void sumaMatrices(int R[num_filas_RGB][num_columnas_RGB], int G[num_filas_RGB][num_columnas_RGB], int B[num_filas_RGB][num_columnas_RGB], int RGB[num_filas_RGB][num_columnas_RGB])
{
    int x = threadIdx.x;
    int y = threadIdx.y;

    //calculamos la fila y la columna
    int fila = blockIdx.y * tile_width + y;
    int columna = blockIdx.x * tile_width + x;

    //sumamos
    RGB[fila][columna] = R[fila][columna] + G[fila][columna] + B[fila][columna];
}

// Matriz volteada 180º
__global__ void matrizVolteada(int a[num_filas_filtro][num_columnas_filtro], int volteada[num_filas_filtro][num_columnas_filtro])
{
    int x = threadIdx.x;
    int y = threadIdx.y;

    //calculamos la fila y la columna
    int fila = blockIdx.y * tile_width + y;
    int columna = blockIdx.x * tile_width + x;

    //matriz volteada
    // [0][0] <--> [2][2]   ||  [1][1] <--> [1][1]
    if (fila == columna)
    {
        volteada[(num_filas_filtro - 1 - fila)][(num_columnas_filtro - 1 - columna)] = a[fila][columna];
    }
    // [0][2] <--> [2][0]
    else if (((fila == 0) && (columna == (num_columnas_filtro - 1))) || ((fila == (num_filas_filtro - 1)) && (columna == 0)))
    {
        volteada[columna][fila] = a[fila][columna];
    }
    // [0][1] <--> [2][1]
    else if (columna == 1)
    {
        volteada[abs(fila - (num_filas_filtro - 1))][columna] = a[fila][columna];
    }
    // [1][0] <--> [1][2]
    else if (fila == 1)
    {
        volteada[fila][abs(columna - (num_columnas_filtro - 1))] = a[fila][columna];
    }
}

// Convolucion
__global__ void convolucion(int a[num_filas_RGB][num_columnas_RGB], int b[num_filas_filtro][num_columnas_filtro], int c[num_filas_RGB][num_columnas_RGB])
{
    int x = threadIdx.x;
    int y = threadIdx.y;

    //calculamos la fila y la columna
    int fila = blockIdx.y * tile_width + y;
    int columna = blockIdx.x * tile_width + x;

    //primera fila
    if (fila == 0)
    {
        //primer elemento de la fila
        if (columna == 0)
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[0][0] * b[1][1]    --> el elemento inicial
                a[fila][columna + 1] * b[1][2] +              // a[0][1] * b[1][2]    --> el elemento de la derecha
                a[fila + 1][columna] * b[2][1] +              // a[1][0] * b[2][1]    --> el elemento de abajo
                a[fila + 1][columna + 1] * b[2][2];           // a[1][1] * b[2][2]    --> el elemento de abajo a la derecha
        }
        //ultimo elemento de la fila
        else if (columna == (num_columnas_RGB - 1))
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[0][15] * b[1][1]    --> el elemento inicial
                a[fila][columna - 1] * b[1][2] +              // a[0][14] * b[1][2]    --> el elemento de la izquierda
                a[fila + 1][columna] * b[2][1] +              // a[1][15] * b[2][1]    --> el elemento de abajo
                a[fila + 1][columna - 1] * b[2][2];           // a[1][14] * b[2][2]    --> el elemento de abajo a la izquierda
        }
        // cualquier otra columna
        else
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[0][a] * b[1][1]        --> el elemento inicial
                a[fila][columna + 1] * b[1][2] +              // a[0][a + 1] * b[1][2]    --> el elemento de la derecha
                a[fila][columna - 1] * b[1][2] +              // a[0][a - 1] * b[1][2]    --> el elemento de la izquierda
                a[fila + 1][columna] * b[2][1] +              // a[1][a] * b[2][1]        --> el elemento de abajo
                a[fila + 1][columna + 1] * b[2][2] +          // a[1][a + 1] * b[2][2]    --> el elemento de abajo a la derecha
                a[fila + 1][columna - 1] * b[2][2];           // a[1][a - 1] * b[2][2]    --> el elemento de abajo a la izquierda
        }
    }
    //ultima fila
    else if (fila == (num_filas_RGB - 1))
    {
        //primer elemento de la fila
        if (columna == 0)
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[15][0] * b[1][1]    --> el elemento inicial
                a[fila][columna + 1] * b[1][2] +              // a[15][1] * b[1][2]    --> el elemento de la derecha
                a[fila - 1][columna] * b[2][1] +              // a[14][0] * b[2][1]    --> el elemento de arriba
                a[fila - 1][columna + 1] * b[2][2];           // a[14][1] * b[2][2]    --> el elemento de arriba a la derecha
        }
        //ultimo elemento de la fila
        else if (columna == (num_columnas_RGB - 1))
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[15][15] * b[1][1]    --> el elemento inicial
                a[fila][columna - 1] * b[1][2] +              // a[15][14] * b[1][2]    --> el elemento de la izquierda
                a[fila - 1][columna] * b[2][1] +              // a[14][15] * b[2][1]    --> el elemento de arriba
                a[fila - 1][columna - 1] * b[2][2];           // a[14][14] * b[2][2]    --> el elemento de arriba a la izquierda
        }
        // cualquier otra columna
        else
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[15][a] * b[1][1]        --> el elemento inicial
                a[fila][columna + 1] * b[1][2] +              // a[15][a + 1] * b[1][2]    --> el elemento de la derecha
                a[fila][columna - 1] * b[1][2] +              // a[15][a - 1] * b[1][2]    --> el elemento de la izquierda
                a[fila - 1][columna] * b[2][1] +              // a[14][a] * b[2][1]        --> el elemento de arriba
                a[fila - 1][columna + 1] * b[2][2] +          // a[14][a + 1] * b[2][2]    --> el elemento de arriba a la derecha
                a[fila - 1][columna - 1] * b[2][2];           // a[14][a - 1] * b[2][2]    --> el elemento de arriba a la izquierda
        }
    }
    //cualquier otra fila
    else
    {
        //primer elemento de la fila
        if (columna == 0)
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[x][a] * b[1][1]            --> el elemento inicial
                a[fila][columna + 1] * b[1][2] +              // a[x][a + 1] * b[1][2]        --> el elemento de la derecha
                a[fila - 1][columna] * b[2][1] +              // a[x - 1][a] * b[2][1]        --> el elemento de arriba
                a[fila - 1][columna + 1] * b[2][2] +          // a[x - 1][a + 1] * b[2][2]    --> el elemento de arriba a la derecha
                a[fila + 1][columna] * b[2][1] +              // a[x + 1][a] * b[2][1]        --> el elemento de abajo
                a[fila + 1][columna + 1] * b[2][2];           // a[x + 1][a + 1] * b[2][2]    --> el elemento de abajo a la derecha
        }
        //ultimo elemento de la fila
        else if (columna == (num_columnas_RGB - 1))
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[x][a] * b[1][1]            --> el elemento inicial
                a[fila][columna - 1] * b[1][2] +              // a[x][a - 1] * b[1][2]        --> el elemento de la izquierda
                a[fila - 1][columna] * b[2][1] +              // a[x - 1][a] * b[2][1]        --> el elemento de arriba
                a[fila - 1][columna - 1] * b[2][2] +          // a[x - 1][a - 1] * b[2][2]    --> el elemento de arriba a la izquierda
                a[fila + 1][columna] * b[2][1] +              // a[x + 1][a] * b[2][1]        --> el elemento de abajo
                a[fila + 1][columna - 1] * b[2][2];           // a[x + 1][a - 1] * b[2][2]    --> el elemento de abajo a la izquierda
        }
        // cualquier otra columna
        else
        {
            c[fila][columna] = a[fila][columna] * b[1][1] +   // a[x][a] * b[1][1]            --> el elemento inicial
                a[fila][columna + 1] * b[1][2] +              // a[x][a + 1] * b[1][2]        --> el elemento de la derecha
                a[fila][columna - 1] * b[1][2] +              // a[x][a - 1] * b[1][2]        --> el elemento de la izquierda
                a[fila - 1][columna] * b[2][1] +              // a[x - 1][a] * b[2][1]        --> el elemento de arriba
                a[fila - 1][columna + 1] * b[2][2] +          // a[x - 1][a + 1] * b[2][2]    --> el elemento de arriba a la derecha
                a[fila - 1][columna - 1] * b[2][2] +          // a[x - 1][a - 1] * b[2][2]    --> el elemento de arriba a la izquierda
                a[fila + 1][columna] * b[2][1] +              // a[x + 1][a] * b[2][1]        --> el elemento de abajo
                a[fila + 1][columna + 1] * b[2][2] +          // a[x + 1][a + 1] * b[2][2]    --> el elemento de abajo a la derecha
                a[fila + 1][columna - 1] * b[2][2];           // a[x + 1][a - 1] * b[2][2]    --> el elemento de abajo a la izquierda
        }
    }
}


//Main
int main()
{
    // ------------------------------------------------------------------
    // 1 - Creamos 3 matrices para despues sumarlas y crear la matriz RGB
    // ------------------------------------------------------------------

    int R[num_filas_RGB][num_columnas_RGB] = {};          // representa a Red
    crearMatriz(R);
    int G[num_filas_RGB][num_columnas_RGB] = {};          // representa a Green
    crearMatriz(G);
    int B[num_filas_RGB][num_columnas_RGB] = {};          // representa a Blue
    crearMatriz(B);
    int RGB[num_filas_RGB][num_columnas_RGB] = {};        // matriz RGB

    int(*r_)[width_RGB];
    int(*g_)[width_RGB];
    int(*b_)[width_RGB];
    int(*rgb_)[width_RGB];

    const int size_RGB = num_filas_RGB * num_columnas_RGB * sizeof(int);

    // Reservamos memoria para las copias de las matrices que pasaremos por memoria del device
    cudaMalloc((void**)&r_, size_RGB);
    cudaMalloc((void**)&g_, size_RGB);
    cudaMalloc((void**)&b_, size_RGB);
    cudaMalloc((void**)&rgb_, size_RGB);

    // Asignamos R, G, B en la memoria del device
    cudaMemcpy(r_, R, size_RGB, cudaMemcpyHostToDevice);
    cudaMemcpy(g_, G, size_RGB, cudaMemcpyHostToDevice);
    cudaMemcpy(b_, B, size_RGB, cudaMemcpyHostToDevice);
    cudaMemcpy(rgb_, RGB, size_RGB, cudaMemcpyHostToDevice);

    // Definimos un bloque bidimensional (coleccion de hilos)
    dim3 dimGrid(width_RGB / tile_width, width_RGB / tile_width);             //cuántos bloques tengo en mi matriz --> 32/2=16 
    dim3 dimBlock(tile_width, tile_width);                                    //cantidad de hilos por bloque --> 2x2 hilos

    // Invocamos al Kernell
    sumaMatrices << <dimGrid, dimBlock >> > (r_, g_, b_, rgb_);

    // Leemos RGB del device
    cudaMemcpy(RGB, rgb_, size_RGB, cudaMemcpyDeviceToHost);

    // Imprimimos la matriz a convolucionar
    cout << "Matriz a convolucionar: " << endl << endl;
    imprimir_matriz(RGB);


    // ------------------------------------------------------------------------
    // 2 - Creamos la matriz FILTRO y después conseguimos su matriz volteada
    // ------------------------------------------------------------------------

    int Filtro[num_filas_filtro][num_columnas_filtro] = { {1,2,3},{4,5,6},{7,8,9} };// { {0,1,0},{1,1,1},{0,1,0} }; 
    int Filtro_volt[num_filas_filtro][num_columnas_filtro];

    int(*filtro_)[width_filtro];
    int(*filtro_volt_)[width_filtro];

    const int size_filtro = num_filas_filtro * num_columnas_filtro * sizeof(int);

    // Reservamos memoria para las copias de las matrices que pasaremos por memoria del device
    cudaMalloc((void**)&filtro_, size_filtro);
    cudaMalloc((void**)&filtro_volt_, size_filtro);

    // Asignamos Filtro en la memoria del device
    cudaMemcpy(filtro_, Filtro, size_filtro, cudaMemcpyHostToDevice);
    cudaMemcpy(filtro_volt_, Filtro_volt, size_filtro, cudaMemcpyHostToDevice);

    // Definimos un bloque bidimensional (coleccion de hilos)
    dim3 dimGrid_filtro(width_filtro / tile_width, width_filtro / tile_width);             //cuántos bloques tengo en mi matriz --> 3/2=1,5 --> 1 bloque
    dim3 dimBlock_filtro(width_filtro, width_filtro);                                      //cantidad de hilos por bloque --> 9 hilos

    // Imprimimos la matriz filtro
    cout << "Matriz filtro inicial: " << endl << endl;
    imprimir_matriz_filtro(Filtro);

    // Invocamos al Kernell 
    matrizVolteada << <dimGrid_filtro, dimBlock_filtro >> > (filtro_, filtro_volt_);

    // Leemos Filtro_volt del device
    cudaMemcpy(Filtro_volt, filtro_volt_, size_filtro, cudaMemcpyDeviceToHost);

    // Imprimimos la matriz filtro volteada
    cout << "Matriz filtro volteada: " << endl << endl;
    imprimir_matriz_filtro(Filtro_volt);


    // --------------------------------------------------------------------------------
    // 3 - Realizamos la convolución de la matriz tras haber creado la matriz resultado
    // --------------------------------------------------------------------------------

    int Resultado[num_filas_RGB][num_columnas_RGB] = {};
    int(*resultado_)[width_RGB];

    // Reservamos memoria para la copia de la matriz que pasaremos por memoria del device
    cudaMalloc((void**)&resultado_, size_RGB);

    // Asignamos Resultado, Filtro_volt, RGB en la memoria del device
    cudaMemcpy(resultado_, Resultado, size_RGB, cudaMemcpyHostToDevice);
    cudaMemcpy(filtro_volt_, Filtro_volt, size_filtro, cudaMemcpyHostToDevice);
    cudaMemcpy(rgb_, RGB, size_RGB, cudaMemcpyHostToDevice);

    // Invocamos al Kernell
    convolucion << <dimGrid, dimBlock >> > (rgb_, filtro_volt_, resultado_);

    // Leemos Resultado del device
    cudaMemcpy(Resultado, resultado_, size_RGB, cudaMemcpyDeviceToHost);

    // Imprimimos la matriz convolucionada
    cout << "Matriz convolucionada: " << endl << endl;
    imprimir_matriz(Resultado);

    // -----------------------------------------------------------------
    // 4 - Liberamos memoria
    // -----------------------------------------------------------------

    cudaFree(r_);
    cudaFree(g_);
    cudaFree(b_);
    cudaFree(rgb_);
    cudaFree(filtro_);
    cudaFree(filtro_volt_);

    return 0;
}


// Creamos la matriz
void crearMatriz(int matriz[num_filas_RGB][num_columnas_RGB])
{
    int nums_random = 255;  //el rango es [0,255]

    for (int i = 0; i < num_filas_RGB; i++)
    {
        for (int j = 0; j < num_columnas_RGB; j++)
        {
            matriz[i][j] = (rand() % nums_random) + 1;
        }
    }
}

// Imprimir matrices 32x32
void imprimir_matriz(int a[num_filas_RGB][num_columnas_RGB])
{
    for (int i = 0; i < num_filas_RGB; i++)
    {
        for (int j = 0; j < num_columnas_RGB; j++)
        {
            if (j == 0)
            {
                cout << "{";
            }

            cout << a[i][j];

            if (j == (num_columnas_RGB - 1))
            {
                cout << "}\n";
            }
            else
            {
                cout << ", ";
            }
        }
    }
    cout << endl << endl;
}

// Imprimir matriz 3x3
void imprimir_matriz_filtro(int a[num_filas_filtro][num_columnas_filtro])
{
    for (int i = 0; i < num_filas_filtro; i++)
    {
        for (int j = 0; j < num_columnas_filtro; j++)
        {
            if (j == 0)
            {
                cout << "{";
            }

            cout << a[i][j];

            if (j == (num_columnas_filtro - 1))
            {
                cout << "}\n";
            }
            else
            {
                cout << ", ";
            }
        }
    }
    cout << endl << endl;
}

