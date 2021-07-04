#include<stdio.h>
#include<cuda.h>
#include <iostream>
using namespace std;

__global__ void addKernel(int* a, int* c)
{
    int x = threadIdx.x;
    int y = threadIdx.y;
    int id_a = blockDim.x * y + x;
    int id_c = blockDim.x * x + y;
    c[id_c] = a[id_a];
}
int main()
{
    const int num_filas = 4;
    const int num_columnas = 4;
    const int a[num_filas][num_columnas] = { { 1, 2, 3, 4 }, { 1, 2, 3, 4 }, { 1, 2, 3, 4 }, { 1, 2, 3, 4 } };
    int c[num_filas][num_columnas] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };
    int* d, * f;

    const int size = num_filas * num_columnas * sizeof(int);

    cudaMalloc((void**)&d, size);
    cudaMalloc((void**)&f, size);
    cudaMemcpy(d, a, size, cudaMemcpyHostToDevice);
    cudaMemcpy(f, c, size, cudaMemcpyHostToDevice);

    dim3 threadBlock(num_columnas, num_filas); // definimos un bloque bidimensional (coleccion de hilos)

    addKernel << <1, threadBlock >> > (d, f);

    cudaMemcpy(c, f, size, cudaMemcpyDeviceToHost);

    cout << "{" << a[0][0] << ", " << a[0][1] << ", " << a[0][2] << ", " << a[0][3] << "}         " << "{" << c[0][0] << ", " << c[0][1] << ", " << c[0][2] << ", " << c[0][3] << "}\n";
    cout << "{" << a[1][0] << ", " << a[1][1] << ", " << a[1][2] << ", " << a[1][3] << "}   -->   " << "{" << c[1][0] << ", " << c[1][1] << ", " << c[1][2] << ", " << c[1][3] << "}\n";
    cout << "{" << a[2][0] << ", " << a[2][1] << ", " << a[2][2] << ", " << a[2][3] << "}         " << "{" << c[2][0] << ", " << c[2][1] << ", " << c[2][2] << ", " << c[2][3] << "}\n";
    cout << "{" << a[3][0] << ", " << a[3][1] << ", " << a[3][2] << ", " << a[3][3] << "}         " << "{" << c[3][0] << ", " << c[3][1] << ", " << c[3][2] << ", " << c[3][3] << "}\n";


    cudaFree(d);
    cudaFree(f);
    return 0;
}

