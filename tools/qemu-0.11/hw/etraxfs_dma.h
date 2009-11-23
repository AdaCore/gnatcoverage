struct etraxfs_dma_client
{
	/* DMA controller. */
	int channel;
	void *ctrl;

	/* client.  */
	struct
	{
		int (*push)(void *opaque, unsigned char *buf, int len);
		void (*pull)(void *opaque);
		void *opaque;
	} client;
};

void *etraxfs_dmac_init(target_phys_addr_t base, int nr_channels);
void etraxfs_dmac_connect(void *opaque, int channel, qemu_irq *line,
			  int input);
void etraxfs_dmac_connect_client(void *opaque, int c, 
				 struct etraxfs_dma_client *cl);
int etraxfs_dmac_input(struct etraxfs_dma_client *client, 
		       void *buf, int len, int eop);
