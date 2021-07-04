#include<stdio.h>
#include<cuda.h>
#include <iostream>
using namespace std;

__global__ void addKernel(int* a, int* b, int* c)
{
    
    int x = threadIdx.x;
    int y = threadIdx.y;
        
    int res = 0;
    for (int k = 0; k < blockDim.x; ++k)
    {
        int num_a = a[blockDim.x * x + k];
        int num_b = b[blockDim.x * k + y];
        res += (num_a * num_b);
    }
    c[blockDim.x * x + y] = res;
        
}
int main()
{
    const int num_filas = 4;
    const int num_columnas = 4;
    const int a[num_filas][num_columnas] = { { 1, 2, 3, 4 }, { 1, 2, 3, 4 }, { 1, 2, 3, 4 }, { 1, 2, 3, 4 } };
    const int b[num_filas][num_columnas] = { { 10, 20, 30, 40 }, { 11, 12, 13, 14 }, { 10, 20, 30, 40 }, { 10, 20, 30, 40 } };
    int c[num_filas][num_columnas] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };
    int* d, * e, * f;

    const int size = num_filas * num_columnas * sizeof(int);

    //asignamos a,b y c en la memoria del device
    cudaMalloc((void**)&d, size);
    cudaMalloc((void**)&e, size);
    cudaMalloc((void**)&f, size);
    cudaMemcpy(d, a, size, cudaMemcpyHostToDevice);
    cudaMemcpy(e, b, size, cudaMemcpyHostToDevice);
    cudaMemcpy(f, c, size, cudaMemcpyHostToDevice);

    dim3 threadBlock(num_columnas, num_filas); // definimos un bloque bidimensional (coleccion de hilos)

    //invocamos al kernel
    addKernel << <1, threadBlock >> > (d, e, f);

    //leemos c del device
    cudaMemcpy(c, f, size, cudaMemcpyDeviceToHost);

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

