/* :vim:ts=4:sw=4:et: */
#ifndef _PARTBIN_H_
#define _PARTBIN_H_

/* bits per position in bitmap */
#ifndef BMAPBPP
# define BMAPBPP 32                             /* unsigned long */
#endif

/* setup bitmap type and bitmap masks */
#if (BMAPBPP == 32)
# define BMAPTYPE unsigned long
# define BMAPBMASK 0xFFFFFFFF
#elif (BMAPBPP == 16)
# define BMAPTYPE unsigned short
# define BMAPBMASK 0xFFFF
#else
# define BMAPTYPE unsigned char
# define BMAPBMASK 0xFF
#endif

/* Number of Bins. Increase "LAST" to allow more part types */
#define PARTBIN_FIRST 0                         /* Should always be zero */
#define PARTBIN_LAST 15                         /* This is the last index number */
#define PARTBIN_MAX (PARTBIN_LAST + 1)          /* Total number of slots available */

typedef enum tagBasePartType {
        PARTTYPE_INVALID=-1,                    /* Should always be negative */
        PARTTYPE_512=0,                         /* 512 bytes */
        PARTTYPE_1024                           /* 1024 bytes */
} BASEPARTTYPE;

/* Calculation to maximizing parts per BMAPBPP */
#define RoundMaxParts(_nn_) (((_nn_) % BMAPBPP) ? ((((_nn_) / BMAPBPP) + 1) * BMAPBPP) : (_nn_))

#ifdef PART_PRIVATE

/* Main structure for managing a Part Bin Allocator */
typedef struct tagPartBin {
        int typePart;                            /* type of parts in this memory bin */
        int sizeParts;                           /* size of individual parts */
        int maxParts;                            /* maximum parts pre-allocated */
        int freeCount;                           /* available free parts */
        int lastFree;                            /* location in bitmap of last alloc */
        int lenBmap;                             /* count of BMAPTYPE in bmap */
        BMAPTYPE *bmap;                          /* bitmap to indicate free/used part */
		int dummy;
        unsigned char *chunk;                    /* contigous bin of memory for parts */
        unsigned char *echunk;                   /* chunk + sizeParts * maxParts */
} PARTBIN;

/* container for iterating through list of PartBin */
typedef struct tagPartBinList {
        PARTBIN *item;
        struct tagPartBinList *next;
        struct tagPartBinList *prev;
} PARTBINLIST;

/* Descriptor for registered parts */
typedef struct tagPartHeader {
        int partType;                            /* memory type of this mem bin */
        long sizePart;                           /* size of parts allocated from this mem bin */
        long partsPerBin;                        /* number of parts within each bucket */
        long maxBins;                            /* limit length of list to this count */
        long countBins;                          /* keep a running count of what's in the list */
        PARTBINLIST *head;                       /* head of iterator through membin */
} PARTHEADER;

#endif /* PART_PRIVATE */

void        PartInitAll(void);
int         PartRegister(int useType, int useSizeBatch, int useSizePart, int useMaxBins);
int         PartExpunge(int useType);
int         PartExpungeAll(void);
void       *PartAlloc(int whichType);
int         PartFree(int whichType, void *whichPart);
long        PartTotalMem();

#endif /* _PARTBIN_H_ */
