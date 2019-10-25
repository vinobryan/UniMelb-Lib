#include "priority_queue.h"
#include "pacman.h"

void heap_init(struct heap* h)
{
	h->count = 0;
	h->size = initial_size;
	h->heaparr = (node_t **) malloc(sizeof(node_t*) * initial_size);
	for(int i = 0; i < initial_size; i++)
	    h->heaparr[i]=NULL;
	
	if(!h->heaparr) {
		printf("Error allocatinga memory...\n");
		exit(-1);
	}

}

void max_heapify(node_t** data, int loc, int count) {
	int left, right, largest;
	node_t* temp;
	left = 2*(loc) + 1;
	right = left + 1;
	largest = loc;
	

	if (left <= count && data[left]->priority > data[largest]->priority) {
		largest = left;
	} 
	if (right <= count && data[right]->priority > data[largest]->priority) {
		largest = right;
	} 
	
	if(largest != loc) {
		temp = data[loc];
		data[loc] = data[largest];
		data[largest] = temp;
		max_heapify(data, largest, count);
	}

}

void heap_push(struct heap* h, node_t* value)
{
	int index, parent;
 
	// Resize the heap if it is too small to hold all the data
	if (h->count == h->size)
	{
		h->size += 1;
		h->heaparr = realloc(h->heaparr, sizeof(node_t) * h->size);
		if (!h->heaparr) exit(-1); // Exit if the memory allocation fails
	}
 	
 	index = h->count++; // First insert at last of array

 	// Find out where to put the element and put it
	for(;index; index = parent)
	{
		parent = (index - 1) / 2;
		if (h->heaparr[parent]->priority >= value->priority) break;
		h->heaparr[index] = h->heaparr[parent];
	}
	h->heaparr[index] = value;
}

void heap_display(struct heap* h) {
	int i;
	for(i=0; i<h->count; ++i) {
	    node_t* n = h->heaparr[i];
	    
	    printf("priority = %d", n->priority);
	    printf("\n");
	    DrawWindowState( n->state );
	}
}

node_t* heap_delete(struct heap* h)
{
	node_t* removed;
	node_t* temp = h->heaparr[--h->count];
 	
	
	if ((h->count <= (h->size + 2)) && (h->size > initial_size))
	{
		h->size -= 1;
		h->heaparr = realloc(h->heaparr, sizeof(node_t) * h->size);
		if (!h->heaparr) exit(-1); // Exit if the memory allocation fails
	}
 	removed = h->heaparr[0];
 	h->heaparr[0] = temp;
	if(temp == removed) h->heaparr[0] = NULL;
 	max_heapify(h->heaparr, 0, h->count);
 	return removed;
}


void emptyPQ(struct heap* pq) {
	while(pq->count != 0) {
		node_t* n = heap_delete(pq);
		free(n);
		//printf("<<%d", heap_delete(pq));
	}
}
