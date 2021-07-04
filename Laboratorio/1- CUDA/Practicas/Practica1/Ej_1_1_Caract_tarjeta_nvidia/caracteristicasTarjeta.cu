#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdio.h>
#include "C:\Users\laura\source\repos\caracteristicasTarjeta\cuda_by_example\common\book.h"
#include <iostream>
#include <iostream>
using namespace std;

int main(void) {
	cudaDeviceProp prop;
	int count;
	HANDLE_ERROR(cudaGetDeviceCount(&count));
	for (int i = 0; i < count; i++) {
		HANDLE_ERROR(cudaGetDeviceProperties(&prop, i));
		printf(" --- Informacion general del dispositivo ---\n");
		cout << "Nombre: " << prop.name << "\n";
		cout << "Memoria global (bytes): " << prop.totalGlobalMem << "\n";
		cout << "Memoria compartida maxima que puede usar un solo bloque (bytes): " << prop.sharedMemPerBlock << "\n";
		cout << "Numero de registros de 32 bits disponibles por bloque: " << prop.regsPerBlock << "\n";
		cout << "Numero de hilos en un wrap: " << prop.warpSize << "\n";
		cout << "Paso maximo permitido para copias de memoria (bytes): " << prop.memPitch << "\n";
		cout << "Numero maximo de hilos que un bloque puede contener: " << prop.maxThreadsPerBlock << "\n";
		cout << "Numero maximo de hilos permitidos por cada dimension del bloque: (" << prop.maxThreadsDim[0] << ", " << prop.maxThreadsDim[1] << ", " << prop.maxThreadsDim[2] << ")\n";
		cout << "Numero de bloques permitidos por cada dimension de la cuadricula: (" << prop.maxGridSize[0] << ", " << prop.maxGridSize[1] << ", " << prop.maxGridSize[2] << ")\n";
		cout << "Memoria constante disponible: " << prop.totalConstMem << "\n";   
		cout << "Revision principal de la capacidad computacional del dispositivo: " << prop.major << "\n";
		cout << "Revision menor de la capacidad computacional del dispositivo: " << prop.minor << "\n";
		cout << "Requisito del dispositivo para la alineación de la textura: " << prop.textureAlignment << "\n";
		cout << "?Puede el dispositivo realizar simultaneamente una ejecucion cudaMemcpy () y del kernel?: ";
		if (prop.deviceOverlap)
		{
			cout << " true\n";
		}
		else
		{
			cout << "false\n";
		}
		cout << "Numero de multiprocesadores en el dispositivo: " << prop.multiProcessorCount << "\n";
		cout << "?Hay un tiempo limite para la ejecucion de los kernels en el dispositivo?: ";
		if (prop.kernelExecTimeoutEnabled)
		{
			cout << " true\n";
		}
		else
		{
			cout << "false\n";
		}
		cout << "?Es el dispositivo un integrante del GPU?: ";
		if (prop.integrated)
		{
			cout << " true\n";
		}
		else
		{
			cout << "false\n";
		}
		cout << "?Puede el dispositivo asignar la memoria del host al espacio de direcciones de CUDA?: ";
		if (prop.canMapHostMemory)
		{
			cout << " true\n";
		}
		else
		{
			cout << "false\n";
		}
	}
	return 0;
}