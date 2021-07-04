#include <stdio.h>
#include <iostream>
#include "cuda_runtime.h"
#include <device_launch_parameters.h>
#include <stdlib.h>
using namespace std;

//variables globales
#define num_filas 16
#define num_columnas 16
#define width 16

void crearMatriz(int matriz[num_filas][num_columnas]);
void imprimir_matrices(int a[num_filas][num_columnas]);


//de muchos a uno
__global__ void addKernel_Gather(int a[16][16], int c[16][16])
{
    int x = threadIdx.x;
    int y = threadIdx.y;
    
    //de la segunda a la penultima posicion
    if ((x > 0) && (x < (num_filas - 1)))
    {
        c[x][y] = a[x][y] + a[x-1][y] + a[x+1][y];
    }
    else
    {
        //primera fila
        if (x == 0)
        {
            c[x][y] = a[x][y] + a[x + 1][y];
        }
        //ultima fila
        else
        {
            c[x][y] = a[x][y] + a[x - 1][y];
        }
    }
    
}

//de uno a muchos
__global__ void addKernel_Scatter(int a[16][16], int c[16][16])
{
    int x = threadIdx.x;
    int y = threadIdx.y;

    //de la segunda a la penultima posicion
    if ((x > 0) && (x < (num_filas - 1)))
    {
        c[x - 1][y] = a[x][y];
        c[x + 1][y] = a[x][y];
    }
    else
    {
        //primera fila
        if (x == 0)
        {
            c[x][y] = a[x][y];
            c[x + 1][y] = a[x][y];
        }
        //ultima fila
        else
        {
            c[x - 1][y] = a[x][y];
            c[x][y] = a[x][y];
        }
    }

}

//de los adyacentes a uno (+)
__global__ void addKernel_Stencil(int a[16][16], int c[16][16])
{
    int x = threadIdx.x;
    int y = threadIdx.y;

    //de la segunda a la penultima posicion
    if ((x > 0) && (y > 0) && (x < (num_columnas - 1)))
    {
        c[x][y-1] = a[x][y];      //izquierda
        c[x][y+1] = a[x][y];      //derecha
        c[x - width][y] = a[x][y];   //arriba
        c[x + width][y] = a[x][y];   //abajo
    }
    else
    {
        //primera columna y primera fila
        if ((y == 0) && (x == 0))
        {
            c[x][y + 1] = a[x][y];      //derecha
            c[x + width][y] = a[x][y];   //abajo
        }
        else
        {
            //ultima columna y ultima fila
            if ((y == (num_columnas - 1)) && (x == (num_filas - 1)))
            {
                c[x][y - 1] = a[x][y];      //izquierda
                c[x - width][y] = a[x][y];   //arriba                
            }
            else
            {
                //primera fila
                if (x == 0)
                {
                    c[x][y - 1] = a[x][y];      //izquierda
                    c[x][y + 1] = a[x][y];      //derecha
                    c[x + width][y] = a[x][y];   //abajo
                }
                else
                {
                    //primera columna
                    if (y == 0)
                    {
                        c[x][y + 1] = a[x][y];      //derecha
                        c[x - width][y] = a[x][y];   //arriba
                        c[x + width][y] = a[x][y];   //abajo
                    }
                    else
                    {
                        //ultima fila
                        if (x == (num_filas - 1))
                        {
                            c[x][y - 1] = a[x][y];      //izquierda
                            c[x][y + 1] = a[x][y];      //derecha
                            c[x - width][y] = a[x][y];   //arriba
                        }
                        else
                        {
                            //ultima columna
                            if (y == (num_filas - 1))
                            {
                                c[x][y - 1] = a[x][y];      //izquierda
                                c[x - width][y] = a[x][y];   //arriba
                                c[x + width][y] = a[x][y];   //abajo
                            }
                        }
                    }
                }
            }
        }
    }

}

int main()
{
    //matriz inicial
    int a[num_filas][num_columnas] = {};
    crearMatriz(a);

    //matriz resultado Gather
    int b[num_filas][num_columnas] = { };
    //matriz resultado Scatter
    int c[num_filas][num_columnas] = { };
    //matriz resultado Stencil
    int d[num_filas][num_columnas] = { };

    int(*e)[16]; //a
    int(*f)[16]; //b
    int(*g)[16]; //c
    int(*h)[16]; //d

    const int size = num_filas * num_columnas * sizeof(int);

    //asignamos a,b y c en la memoria del device
    cudaMalloc((void**)&e, size); 
    cudaMalloc((void**)&f, size);
    cudaMalloc((void**)&g, size);
    cudaMalloc((void**)&h, size);

    cudaMemcpy(e, a, size, cudaMemcpyHostToDevice);
    cudaMemcpy(f, b, size, cudaMemcpyHostToDevice);
    cudaMemcpy(g, c, size, cudaMemcpyHostToDevice);
    cudaMemcpy(h, d, size, cudaMemcpyHostToDevice);

    //execution configuration
    dim3 dimBlock(1,1); //es 1 bloque
    dim3 threads(num_columnas, num_filas);  //16 hilos


    //invocamos al kernel
    addKernel_Gather << <dimBlock, threads>> > (e, f);
    addKernel_Scatter << <dimBlock, threads>> > (e, g);
    addKernel_Stencil << <dimBlock, threads>> > (e, h);

    //leemos c del device
    cudaMemcpy(b, f, size, cudaMemcpyDeviceToHost);
    cudaMemcpy(c, g, size, cudaMemcpyDeviceToHost);
    cudaMemcpy(d, h, size, cudaMemcpyDeviceToHost);

    //imprimimos matrices
    cout << "Matriz inicial: \n\n";
    imprimir_matrices(a);
    cout << "\n\nGather: \n\n";
    imprimir_matrices(b);
    cout << "\n\nScatter: \n\n";
    imprimir_matrices(c);
    cout << "\n\nStencil: \n\n";
    imprimir_matrices(d);
    
    //liberamos memoria
    cudaFree(e);
    cudaFree(f);
    cudaFree(g);
    cudaFree(h);
    return 0;
}

//creamos la matriz
void crearMatriz(int matriz[num_filas][num_columnas])
{
    int nums_random = 99;

    for (int i = 0; i < num_filas; i++)
    {
        for (int j = 0; j < num_columnas; j++)
        {
            matriz[i][j] = (rand() % nums_random) + 1;
        }
    }
}

//imprimir matrices
void imprimir_matrices(int a[num_filas][num_columnas])
{
    for (int i = 0; i < num_filas; i++)
    {
        for (int j = 0; j < num_columnas; j++)
        {
            if (j == 0)
            {
                cout << "{";
            }

            cout << a[i][j];

            if (j == (num_columnas - 1))
            {
                cout << "}\n";
            }
            else
            {
                cout << ", ";
            }
        }
    }
}