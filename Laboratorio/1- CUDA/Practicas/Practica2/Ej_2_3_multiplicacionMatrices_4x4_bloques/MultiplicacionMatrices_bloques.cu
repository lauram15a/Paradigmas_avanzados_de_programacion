#include <stdio.h>
#include <iostream>
#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdlib.h>
using namespace std;

__global__ void addKernel(int a[4][4], int b[4][4], int c[4][4], int size, int tile_width)
{
    int x = threadIdx.x;
    int y = threadIdx.y;

    //calculamos la fila index del elemento c y a
    int fila = blockIdx.x * tile_width + x;

    //calculamos la columna index del elemento c y b
    int columna = blockIdx.y * tile_width + y;

    int res = 0;

    //cada hilo calcula un elemento del bloque sub-matriz
    for (int k = 0; k < size; ++k)
    {
        c[fila][columna] += a[fila][k] * b[k][columna];
    }
}

int main()
{
    const int num_filas = 4;
    const int num_columnas = 4;
    const int a[num_filas][num_columnas] = { { 1, 2, 3, 4 }, { 1, 2, 3, 4 }, { 1, 2, 3, 4 }, { 1, 2, 3, 4 } };
    const int b[num_filas][num_columnas] = { { 10, 20, 30, 40 }, { 11, 12, 13, 14 }, { 10, 20, 30, 40 }, { 10, 20, 30, 40 } };
    int c[num_filas][num_columnas] = { };

    int(*d)[4];
    int(*e)[4];
    int(*f)[4];

    const int size = num_filas * num_columnas * sizeof(int);
    const int tile_width = 2;

    //asignamos a,b y c en la memoria del device
    cudaMalloc((void**)&d, size);
    cudaMalloc((void**)&e, size);
    cudaMalloc((void**)&f, size);
    cudaMemcpy(d, a, size, cudaMemcpyHostToDevice);
    cudaMemcpy(e, b, size, cudaMemcpyHostToDevice);
    cudaMemcpy(f, c, size, cudaMemcpyHostToDevice);

    //execution configuration
    dim3 dimGrid(size / tile_width, size / tile_width);
    dim3 dimBlock(tile_width, tile_width);

    //invocamos al kernel
    addKernel << <dimGrid, dimBlock >> > (d, e, f, size, tile_width);  

    //leemos c del device
    cudaMemcpy(c, f, size, cudaMemcpyDeviceToHost);

    //imprimimos matrices
    cout << "{" << a[0][0] << ", " << a[0][1] << ", " << a[0][2] << ", " << a[0][3] << "}       " << "{" << b[0][0] << ", " << b[0][1] << ", " << b[0][2] << ", " << b[0][3] << "}       " << "{" << c[0][0] << ", " << c[0][1] << ", " << c[0][2] << ", " << c[0][3] << "}\n";
    cout << "{" << a[1][0] << ", " << a[1][1] << ", " << a[1][2] << ", " << a[1][3] << "}   *   " << "{" << b[1][0] << ", " << b[1][1] << ", " << b[1][2] << ", " << b[1][3] << "}   =   " << "{" << c[1][0] << ", " << c[1][1] << ", " << c[1][2] << ", " << c[1][3] << "}\n";
    cout << "{" << a[2][0] << ", " << a[2][1] << ", " << a[2][2] << ", " << a[2][3] << "}       " << "{" << b[2][0] << ", " << b[2][1] << ", " << b[2][2] << ", " << b[2][3] << "}       " << "{" << c[2][0] << ", " << c[2][1] << ", " << c[2][2] << ", " << c[2][3] << "}\n";
    cout << "{" << a[3][0] << ", " << a[3][1] << ", " << a[3][2] << ", " << a[3][3] << "}       " << "{" << b[3][0] << ", " << b[3][1] << ", " << b[3][2] << ", " << b[3][3] << "}       " << "{" << c[3][0] << ", " << c[3][1] << ", " << c[3][2] << ", " << c[3][3] << "}\n";

    //liberamos memoria
    cudaFree(d);
    cudaFree(e);
    cudaFree(f);
    return 0;
}

