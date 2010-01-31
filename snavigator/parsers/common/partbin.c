/* :vim:ts=4:sw=4:et: */
/*
 * Parts allocator from bins of parts:
 *     Mini library to manage chunks of memory into smaller parts
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define PART_PRIVATE    /* expose private parts (nasty!) of library */
#include "partbin.h"

/* Descriptors for all Parts */
static PARTHEADER *partBin = NULL;
static long totalMem = 0;


long
PartTotalMem()
{
    return(totalMem);
}

/* Init all the headers for the parts bins */
void
PartInitAll(void)
{
    int idx;

    if (partBin)
    {
        /* fprintf(stderr, "PartInitAll() already initialized?\n"); */
        return;
    }
    partBin = malloc(sizeof(PARTHEADER) * PARTBIN_MAX);
    totalMem += sizeof(PARTHEADER) * PARTBIN_MAX;
    for (idx = PARTBIN_FIRST; idx < PARTBIN_MAX; idx++)
    {
        partBin[idx].partType = PARTTYPE_INVALID;
        partBin[idx].sizePart = 0;
        partBin[idx].partsPerBin = 0;
        partBin[idx].maxBins = 0;
        partBin[idx].countBins = 0;
        partBin[idx].head = NULL;
    }
}

/* Set a bit in a bitmap */
static void
SetBit(BMAPTYPE *useBmap, int maxBits, int usePos)
{
    int indexPos;
    int bitPos;

    if ( !useBmap )
        return;
    if (usePos >= maxBits)
        return;
    indexPos = usePos / BMAPBPP;
    bitPos = usePos % BMAPBPP;
    useBmap[indexPos] |= (1 << bitPos);
}

/* Clear a bit in a bitmap */
static void
ClearBit(BMAPTYPE *useBmap, int maxBits, int usePos)
{
    int indexPos;
    int bitPos;

    if ( !useBmap )
        return;
    if (usePos >= maxBits)
        return;
    indexPos = usePos / BMAPBPP;
    bitPos = usePos % BMAPBPP;
    useBmap[indexPos] &= ~(1 << bitPos);
}

/* Determine value of a bit in a bitmap */
static int
IsBitSet(BMAPTYPE *useBmap, int maxBits, int usePos)
{
    int indexPos;
    int bitPos;

    if ( !useBmap )
        return(0);
    if (usePos >= maxBits)
        return(0);
    indexPos = usePos / BMAPBPP;
    bitPos = usePos % BMAPBPP;
    return((useBmap[indexPos] & (1 << bitPos)) ? 1 : 0);
}

/*
 * If bit in bitmap is zero, mark it and return true,
 * else if bit in bitmap is one, return false.
 */
#if 0
static int
ReserveBit(BMAPTYPE *useBmap, int maxBits, int usePos)
{
    int bitPos;
    int indexPos;

    if ( !useBmap )
        return(0);
    if (usePos >= maxBits)
        return(0);
    indexPos = usePos / BMAPBPP;
    bitPos = usePos % BMAPBPP;
    if ( !(useBmap[indexPos] & (1 << bitPos)) )
    {
        useBmap[indexPos] |= (1 << bitPos);
        return(1);
    }
    return(0);
}
#endif

/* search bitmap for a free slot, else return -1 */
static int
FindFreeMarkBit(BMAPTYPE *useBmap, int maxBits, int startPos)
{
    int bitPos;
    int indexPos;
    int bitOff;

    if ( !useBmap )
        return(-1);
    if (startPos >= maxBits)
        return(-1);
    if (startPos < 0)
        startPos = 0;
	for(;;)
	{
		for(bitPos = startPos; bitPos < maxBits;)
		{
			indexPos = bitPos / BMAPBPP;
			bitOff = bitPos % BMAPBPP;
			if ( !bitOff )
			{
				if (useBmap[indexPos] == BMAPBMASK)
				{
					bitPos += BMAPBPP;
					continue;
				}
			}
			if ( !(useBmap[indexPos] & (1 << bitOff)) )
			{
				useBmap[indexPos] |= (1 << bitOff);
				return(bitPos);
			}
			bitPos++;
		}
		if (startPos)
		{
			maxBits = startPos;
			startPos = 0;
		}
		else
			break;
	}
    return(-1);
}


/*
 * Create a bin for parts
 */
static PARTBIN *
PartBinAlloc(int useTypePart, int useNumParts, int useSizePart)
{
    PARTBIN *tBin = NULL;
    BMAPTYPE *tMap = NULL;
    unsigned char *tChunk = NULL;
    int maxPos;
    int idx;

    /* ensure that the request is valid */
    if ((useNumParts <= 0) || (useSizePart <= 0))
    {
        fprintf(stderr, "PartBinAlloc() with invalid allocation size\n");
        return(NULL);
    }

    /* allocate the holding structure */
    tBin = (PARTBIN *)malloc(sizeof(PARTBIN));
    if ( !tBin )
    {
        fprintf(stderr, "PartBinAlloc() no mem for bin.\n");
        return(NULL);
    }
    totalMem += sizeof(PARTBIN);

    /* determine number of holding positions in bitmap */
    maxPos = (useNumParts / BMAPBPP);
    if (useNumParts - (maxPos * BMAPBPP))
        maxPos++;

    /* allocate the bitmap */
    tMap = (BMAPTYPE *)malloc(maxPos * sizeof(BMAPTYPE));
    if ( !tMap )
    {
        fprintf(stderr, "PartBinAlloc() no mem for bitmap.\n");
        free((char *)tBin);
        totalMem -= sizeof(PARTBIN);
        return(NULL);
    }
    totalMem += maxPos * sizeof(BMAPTYPE);

    /* allocate the free parts */
    tChunk = (unsigned char *)malloc(useNumParts * useSizePart);
    if ( !tChunk )
    {
        fprintf(stderr, "PartBinAlloc() no mem for memory chunk.\n");
        free((char *)tBin);
        totalMem -= sizeof(PARTBIN);
        free((char *)tMap);
        totalMem -= maxPos * sizeof(BMAPTYPE);
        return(NULL);
    }
    totalMem += useNumParts * useSizePart;
#if 0
    /* fill the chunk */
    memset(tChunk, 0xFF, useNumParts * useSizePart);
#endif

    /* zero out the bitmap */
    for (idx = 0; idx < maxPos; idx++)
        tMap[idx] = 0;

    /* fill in the blanks for the bin */
    tBin->typePart = useTypePart;
    tBin->sizeParts = useSizePart;
    tBin->maxParts = useNumParts;
    tBin->lastFree = -1;
    tBin->freeCount = useNumParts;
    tBin->lenBmap = maxPos;
    tBin->bmap = tMap;
    tBin->chunk = tChunk;
    tBin->echunk = tChunk + (useSizePart * useNumParts);

    return(tBin);
}


/*
 * Free memory usage used by a bin of parts
 */
static void
PartBinFree(PARTBIN *useMem)
{
    int idx;

    /* make sure request is valid */
    if ( !useMem )
        return;
    if (useMem->maxParts <= 0)
        return;

    if (useMem->chunk)
    {
        /* check that all parts have been freed */
        /* note: requires unused bits in bmap be init zeroed */
        if (useMem->bmap)
        {
            for (idx = 0; idx < useMem->lenBmap; idx++)
            {
                if (useMem->bmap[idx])
                {
                    fprintf(stderr, "***Parts not yet freed\n");
                    break;
                }
            }
        }
        /* get rid of the parts */
        free((char *)useMem->chunk);
    	totalMem -= useMem->maxParts * useMem->sizeParts;
        useMem->chunk = NULL;
    }

    /* get rid of the bitmap */
    if (useMem->bmap)
    {
        free((char *)useMem->bmap);
    	totalMem -= useMem->lenBmap * sizeof(BMAPTYPE);
        useMem->bmap = NULL;
    }
    /* get rid of the structure */
    free((char *)useMem);
   	totalMem -= sizeof(PARTBIN);
}


/*
 * Allocate an iterator, attach a bin of parts to it,
 * then insert the iterator into the list.
 */
static PARTBINLIST *
PartBinAttach(PARTBINLIST **useIter, PARTBIN *useBin)
{
    PARTBINLIST *tIter;

    /* make sure the request is valid */
    if ( !useIter )
    {
        fprintf(stderr, "PartBinAttach() invalid bin.\n");
        return(NULL);
    }
    if ( !useBin )
    {
        fprintf(stderr, "PartBinAttach() iterator item is NULL.\n");
        return(NULL);
    }

    /* allocate the iterator structure */
    tIter = (PARTBINLIST *)malloc(sizeof(PARTBINLIST));
    if ( !tIter )
    {
        fprintf(stderr, "PartBinAttach() no mem.\n");
        return(NULL);
    }
    totalMem += sizeof(PARTBINLIST);

    /* initialize the iterator structure */
    tIter->item = useBin;
    tIter->next = NULL;
    tIter->prev = NULL;

    /* If the list is not empty, insert in list */
    if (*useIter)
    {
        tIter->next = *useIter;
        tIter->prev = (*useIter)->prev;
        (*useIter)->prev = tIter;
        if (tIter->prev)
        {
            tIter->prev->next = tIter;
        }
    }
    *useIter = tIter;
    return(tIter);
}

/*
 * Detach a bin of parts from an iterator class and
 * free the iterator. Returns the pointer to the bin
 * of parts.
 */
static PARTBIN *
PartBinDetach(PARTBINLIST **useIter)
{
    PARTBINLIST *tIter = NULL;
    PARTBIN *tItem = NULL;

    /* make sure request is valid */
    if (!useIter || !*useIter)
        return(NULL);

    /* point to target in list */
    tIter = *useIter;
    if (tIter->prev)
    {
        /* fix forward list pointer */
        tIter->prev->next = tIter->next;
    }
    if (tIter->next)
    {
        /* fix backward list pointer */
        tIter->next->prev = tIter->prev;
    }
    /* change current pointer to either prev, next, or NULL */
    if (tIter->prev)
        *useIter = tIter->prev;
    else if (tIter->next)
        *useIter = tIter->next;
    else
        *useIter = NULL;

    /* cut bin from list */
    tIter->next = NULL;
    tIter->prev = NULL;

    /* detach item from iterator */
    tItem = tIter->item;
#if 0
    if ( !tItem )
        fprintf(stderr, "PartBinDetach() warning item is NULL.\n");
#endif
    tIter->item = NULL;
    /* free the iterator */
    free((char *)tIter);
   	totalMem -= sizeof(PARTBINLIST);

    /* return the item */
    return(tItem);
}


/*
 * Register for a memory allocator type.
 * Allocator will allocate a fixed size of memory according to mem type.
 * Allocator will optimize request by reserving batches of mem type.
 * Returns:
 *     index in array of partBin
 *     or
 *     negative integer if invalid memory type
 */
int
PartRegister(int useType, int useSizeBatch, int useSizePart, int useMaxBins)
{
    int idx;

    if ( !partBin )
        PartInitAll();
    /* make sure the request is valid */
    if (useType < 0)
    {
        fprintf(stderr, "PartRegister() invalid id %d\n", useType);
        return(-1);
    }

    /* make sure part has not yet been registered */
    for (idx = PARTBIN_FIRST; idx < PARTBIN_LAST; idx++)
    {
        if (partBin[idx].partType == useType)
        {
#if 0
            fprintf(stderr, "PartRegister() part type %d already registeerd\n", useType);
#endif
            return(idx);
        }
    }

    for (idx = PARTBIN_FIRST; idx < PARTBIN_LAST; idx++)
    {
        if (partBin[idx].partType == PARTTYPE_INVALID)
        {
            /* Put in new entry */
            partBin[idx].partType = useType;
            partBin[idx].sizePart = useSizePart;
            partBin[idx].partsPerBin = useSizeBatch;
            if (useMaxBins < 0)
                partBin[idx].maxBins = INT_MAX;
            else
                partBin[idx].maxBins = useMaxBins;
            partBin[idx].countBins = useSizeBatch;
            partBin[idx].head = NULL;

            return(idx);
        }
    }
    return(-1);
}


/*
 * expunge all parts of this type and deregister part
 */
int
PartExpunge(int useType)
{
    static PARTBIN *tMem;
    int idx;
    int seenNum = 0;

    if ( !partBin )
    {
        PartInitAll();
        return(1);
    }

    for(idx = PARTBIN_FIRST; idx < PARTBIN_MAX; idx++)
    {
        if (partBin[idx].partType == useType)
        {
            tMem = PartBinDetach(&(partBin[idx].head));
            PartBinFree(tMem);

            /* clear out head entry */
            partBin[idx].partType = PARTTYPE_INVALID;
            partBin[idx].sizePart = 0;
            partBin[idx].partsPerBin = 0;
            partBin[idx].maxBins = 0;
            partBin[idx].countBins = 0;
            partBin[idx].head = NULL;
        }
    }

    return(0);
}

/*
 * expunge all parts
 */
int
PartExpungeAll(void)
{
    int idx;
    int errFlag = 0;
    if ( !partBin )
    {
        PartInitAll();
        return(1);
    }
    for (idx = PARTBIN_FIRST; idx < PARTBIN_MAX; idx++)
    {
        int retCode;
        if (partBin[idx].partType > -1)
            retCode = PartExpunge(partBin[idx].partType);
        if (!errFlag && retCode)
            errFlag = retCode;
    }
    return(errFlag);
}


/*
 * allocate part from a bin
 */
static void *
PartAllocFromBin(PARTBIN *useBin, int clearFlag)
{
    int foundFree = 0;
    unsigned char *usePart = NULL;

    if ( !useBin )
    {
        fprintf(stderr, "PartAllocFromBin() alloc from a NULL bin\n");
        return(NULL);
    }
    if (useBin->freeCount <= 0)
    {
        fprintf(stderr, "PartAllocFromBin() alloc from an empty bin\n");
        return(NULL);
    }

    foundFree = FindFreeMarkBit(useBin->bmap, useBin->maxParts, useBin->lastFree);
    if (foundFree < 0)
    {
        useBin->lastFree = -1;
        return(NULL);
    }
    else if (foundFree > useBin->maxParts)
    {
        fprintf(stderr, "PartAllocFromBin() tried alloc beyond bin (bit=%d)\n", foundFree);
        useBin->lastFree = -1;
        return(NULL);
    }
    else
    {
        useBin->lastFree = foundFree;
    }

    /* point to part */
    usePart = useBin->chunk + (foundFree * useBin->sizeParts);

    /* reduce free count */
    useBin->freeCount--;

    /* Clear memory if requested */
    if (clearFlag)
        memset(usePart, 0, useBin->sizeParts);

    /* return part */
    return((void *)usePart);
}



/*
 * free part from a bin
 */
static int
PartFreeFromBin(PARTBIN *useBin, void *whichPart, int clearFlag)
{
    int useIdx;
    unsigned char *usePart = (unsigned char *)whichPart;
    if ( !usePart )
    {
        fprintf(stderr, "PartFreeFromBin() free NULL mem\n");
        return(-1);
    }
    if ( !useBin )
    {
        fprintf(stderr, "PartFreeFromBin() free mem from a NULL bin\n");
        return(-2);
    }
    if (useBin->freeCount >= useBin->maxParts)
    {
        fprintf(stderr, "PartFreeFromBin() free mem from a full bin\n");
        return(-3);
    }
    if ( !useBin->bmap )
    {
        fprintf(stderr, "PartFreeFromBin() NULL bitmap\n");
        return(-4);
    }
    if (usePart < useBin->chunk)
    {
        fprintf(stderr, "PartFreeFromBin() free mem below range.\n");
        return(-5);
    }
    if (usePart > useBin->echunk)
    {
        fprintf(stderr, "PartFreeFromBin() free mem above range.\n");
        return(-6);
    }
    if (useBin->sizeParts <= 0)
    {
        fprintf(stderr, "PartFreeFromBin() free mem bad parts size.\n");
        return(-7);
    }

    /* Determine bit in bitmap to free */
    useIdx = (int)(usePart - useBin->chunk);
    if (useIdx % useBin->sizeParts)
    {
        fprintf(stderr, "PartFreeFromBin() free mem unaligned part index.\n");
        return(-8);
    }
    useIdx /= useBin->sizeParts;
    if (useIdx < 0)
    {
        fprintf(stderr, "PartFreeFromBin() free mem index out of range (bit=%d)\n", useIdx);
        return(-9);
    }
    ClearBit(useBin->bmap, useBin->maxParts, useIdx);

    /* Increment free parts counter */
    useBin->freeCount++;

    /* Clear memory if requested */
    if (clearFlag)
        memset(usePart, 0, useBin->sizeParts);
    return(0);
}


/*
 * allocate part from bin of this type.
 * The type must be registered before use
 */
void *
PartAlloc(int whichType)
{
    int idx;
    void *useMem = NULL;
    PARTHEADER *tPartType = NULL;
    PARTBINLIST *tList = NULL;
    PARTBIN *tBin = NULL;

    if ( !partBin )
        PartInitAll();
    for (idx = PARTBIN_FIRST; idx < PARTBIN_MAX; idx++)
    {
        if (partBin[idx].partType == whichType)
        {
            tPartType = &partBin[idx];
            for (tList = partBin[idx].head; tList; tList = tList->next)
            {
                if ( !tList->item )
                {
#if 0
            		fprintf(stderr, "PartAlloc() missing chunk from part bin\n");
#endif
                    continue;
                }
                if (tList->item->freeCount > 0)
                {
                    useMem = PartAllocFromBin(tList->item, 0);
                    if (useMem)
                        return(useMem);
                }
            }
        }
    }

    if ( !tPartType )
    {
        fprintf(stderr, "PartAlloc() part %d is not registered\n", whichType);
        return(NULL);
    }
    tBin = PartBinAlloc(whichType, tPartType->partsPerBin, tPartType->sizePart);
    if ( !tBin )
        return(NULL);
    PartBinAttach(&tPartType->head, tBin);
    useMem = PartAllocFromBin(tBin, 0);
    return(useMem);
}


/*
 * free part from bin of this type
 */ 
int
PartFree(int whichType, void *whichPart)
{
    /*
     * search for list of requested type
     * if list not found then error
     * for each item on list
     *     if part is in chunk then free part
     *  else move to next chunk on list
     * if part is not in any chunk then failure
     */

    int idx;
    void *useMem = NULL;
    PARTHEADER *tPartType = NULL;
    PARTBINLIST *tList = NULL;
    PARTBIN *tBin = NULL;
    unsigned char *tPart = (unsigned char *)whichPart;

    if ( !partBin )
        return(-1);

    for (idx = PARTBIN_FIRST; idx < PARTBIN_MAX; idx++)
    {
        if (partBin[idx].partType == whichType)
        {
            tPartType = &partBin[idx];
            for (tList = tPartType->head; tList; tList = tList->next)
            {
                if ( !tList->item )
                {
#if 0
            		fprintf(stderr, "PartFree() missing chunk from part bin\n");
#endif
                    continue;
                }
                if (tList->item->freeCount > 0)
                {
                    tBin = tList->item;
                    if ((tPart >= tBin->chunk) && (tPart < tBin->echunk))
                    {
                        PartFreeFromBin(tBin, tPart, 0);    /* free but don't erase part */
                        return(0);
                    }
                }
            }
        }
    }

    return(-1);
}




#ifdef TESTMODULE

/* =================== SELF TEST FUNCTIONS ==================== */

static void
PrintPartBin(PARTBIN *useBin, int numTabs)
{
    int idx;
    char preTabs[10] = "";
    if (numTabs)
    {
        for (idx = 0; idx < sizeof(preTabs) - 1; idx++)
        {
            preTabs[idx] = 0;
            if (idx == numTabs)
                break;
            preTabs[idx] = '\t';
        }
    }
    printf("%s==== Begin Dump PartBin (0x%lx)====\n", preTabs, useBin);
    if ( !useBin )
    {
        fprintf(stderr, "%sPrintPartBin() pointer is NULL\n", preTabs);
        printf("%s==== End Dump PartBin ====\n\n", preTabs);
        return;
    }
    printf("%spart type = %d\n", preTabs, useBin->typePart);
    printf("%spart size = %d\n", preTabs, useBin->sizeParts);
    printf("%smax parts = %d\n", preTabs, useBin->maxParts);
    printf("%sfree parts = %d\n", preTabs, useBin->freeCount);
    printf("%slast free = %d\n", preTabs, useBin->lastFree);
    printf("%slen bmap = %d\n", preTabs, useBin->lenBmap);
    printf("%saddr bmap = 0x%lx\n", preTabs, useBin->bmap);
    printf("%saddr chunk = 0x%lx\n", preTabs, useBin->chunk);
    printf("%saddr echunk = 0x%lx\n", preTabs, useBin->echunk);
    printf("%s==== End Dump PartBin ====\n\n", preTabs);
}

static void
PrintBinList(PARTBINLIST *useIter)
{
    PARTBINLIST *tIter;
    if ( !useIter )
    {
        fprintf(stderr, "PrintBinList() pointer is NULL\n");
        return;
    }
    printf("==== Begin Dump List of PartBins ====\n");
    for(tIter = useIter; tIter; tIter = tIter->next)
    {
        printf("\t<addr iterator = 0x%lx, ", tIter);
        printf("addr item = 0x%lx, ", tIter->item);
        printf("addr next = 0x%lx, ", tIter->next);
        printf("addr prev = 0x%lx>\n", tIter->prev);
        PrintPartBin(tIter->item, 1);
    }
    printf("==== End Dump List of PartBins ====\n\n");
}

static void
PrintPartTypes(int validOnlyFlag)
{
    int idx;

    printf("==== Begin Dump %spart types ====\n", (validOnlyFlag) ? "Valid " : "");
    for (idx = PARTBIN_FIRST; idx < PARTBIN_MAX; idx++)
    {
        if (validOnlyFlag)
            if (partBin[idx].partType == PARTTYPE_INVALID)
                continue;
        printf("partBin[%d].partType = %d\n", idx, partBin[idx].partType);
        printf("partBin[%d].sizePart = %d\n", idx, partBin[idx].sizePart);
        printf("partBin[%d].partsPerBin = %d\n", idx, partBin[idx].partsPerBin);
        printf("partBin[%d].maxBins = %d\n", idx, partBin[idx].maxBins);
        printf("partBin[%d].countBins = %d\n", idx, partBin[idx].countBins);
        printf("partBin[%d].head = 0x%lx\n", idx, partBin[idx].head);
    }
    printf("==== End Dump %spart types ====\n\n", (validOnlyFlag) ? "Valid " : "");
}

#define PrintValidPartTypes() PrintPartTypes(1)
#define PrintAllPartTypes() PrintPartTypes(0)

unsigned char *charPart[1024];

main()
{
    int idx;
    int tmpId;
    /* unsigned char **charPart; */
    int maxParts;
    int partTypeNow = 0;
    PARTBINLIST *tIter = NULL;
    PARTBIN *xBin = NULL;
    PARTBIN *tBin1 = NULL;
    PARTBIN *tBin2 = NULL;
    unsigned char *tPtr1=NULL;
    unsigned char *tPtr2=NULL;
    unsigned char *tPtr3=NULL;

    PartInitAll();

    printf("###### TEST creating first bin. Current Mem=%ld\n", PartTotalMem());
    tBin1 = PartBinAlloc(1, 100, 18);
    PrintPartBin(tBin1, 0);

    printf("###### TEST adding first bin in list. Current Mem=%ld \n", PartTotalMem());
    PartBinAttach(&tIter, tBin1);
    if ( !tIter )
    {
        fprintf(stderr, "Oops, unable to add first partbin in queue\n");
        return(-1);
    }
    else
    {
        PrintBinList(tIter);
    }


    printf("###### TEST creating second bin. Current Mem=%ld\n", PartTotalMem());
    tBin2 = PartBinAlloc(2, 96, 18);
    PrintPartBin(tBin2, 0);

    printf("###### TEST adding second bin in list. Current Mem=%ld\n", PartTotalMem());
    PartBinAttach(&tIter, tBin2);
    if ( !tIter )
    {
        fprintf(stderr, "Oops, unable to add second partbin in queue\n");
        return(-1);
    }
    else
    {
        PrintBinList(tIter);
        xBin = tIter->item;
        if (xBin->typePart != 2)
        {
            fprintf(stderr, "Oops, failed to add second partbin in queue\n");
            return(-1);
        }
    }


    printf("###### TEST detaching second bin in list. Current Mem=%ld\n", PartTotalMem());
    printf("\n\t{ List Before detaching } \n");
    PrintBinList(tIter);
    xBin = PartBinDetach(&tIter);
    if (xBin->typePart != 2)
    {
        fprintf(stderr, "Oops, failed to dequeu second added partbin in queue\n");
        return(-1);
    }
    printf("\n\t{ Detached Bin } \n");
    PrintPartBin(xBin, 0);
    printf("\n\t{ List After detaching } \n");
    PrintBinList(tIter);


    /* Test detach first bin in list */
    printf("###### TEST detaching first bin in list. Current Mem=%ld\n", PartTotalMem());
    printf("\n\t{ List Before detaching } \n");
    PrintBinList(tIter);
    printf("\n\t{ Detached Bin } \n");
    xBin = PartBinDetach(&tIter);
    PrintPartBin(xBin, 0);
    if (xBin->typePart != 1)
    {
        fprintf(stderr, "Oops, failed to dequeu first added partbin in queue\n");
        return(-1);
    }
    if (tIter)
    {
        fprintf(stderr, "Oops, list not empty!\n");
        PrintBinList(tIter);
    }



    /*
    * Test partbin part allocation
    */

    printf("###### TEST allocating one part from a bin of parts. Current Mem=%ld\n", PartTotalMem());
    /* charPart = (unsigned char **)malloc(sizeof(unsigned char *) * xBin->maxParts); */
    charPart[0] = (unsigned char *)PartAllocFromBin(xBin, 0);
    PrintPartBin(xBin, 0);

    printf("###### TEST allocating remaining free parts from a bin of parts. Current Mem=%ld\n", PartTotalMem());
    for (idx = 1; idx < xBin->maxParts; idx++)
    {
        if (xBin->freeCount <= 0)
        {
            fprintf(stderr, "xBin->freeCount (%d) <= 0\n", xBin->freeCount);
            break;
        }
        charPart[idx] = (unsigned char *)PartAllocFromBin(xBin, 0);
    }
    PrintPartBin(xBin, 0);


    printf("###### TEST freeing one part from a bin of parts. Current Mem=%ld\n", PartTotalMem());
    PartFreeFromBin(xBin, charPart[0], 0);
    charPart[0] = NULL;
    PrintPartBin(xBin, 0);

    printf("###### TEST freeing remaining parts from a bin of parts. Current Mem=%ld\n", PartTotalMem());
    PrintPartBin(xBin, 0);
    for (idx = 1; idx < xBin->maxParts; idx++)
    {
        if (xBin->freeCount > xBin->maxParts)
        {
            fprintf(stderr, "xBin->freeCount (%d) > xBin->maxParts (%d) \n",
                xBin->freeCount, xBin->maxParts);
            break;
        }
        PartFreeFromBin(xBin, charPart[idx], 0);
        charPart[idx] = NULL;
    }
    PrintPartBin(xBin, 0);

    printf("###### TEST freeing remaining bins . Current Mem=%ld\n", PartTotalMem());
	PartBinFree(tBin1);
    printf("\n\t{ After free tBin1. Current Mem=%ld }\n", PartTotalMem());
	PartBinFree(tBin2);
    printf("\n\t{ After free tBin2. Current Mem=%ld }\n", PartTotalMem());

    /*
    * Test part type allocation and free
    */

    partTypeNow = 3;
    printf("###### TEST registering first part type %d. Current Mem=%ld\n", partTypeNow, PartTotalMem());
    tmpId = PartRegister(partTypeNow, 100, sizeof(PARTHEADER), 16);
    PrintValidPartTypes();

    partTypeNow = 4;
    printf("###### TEST registering second part type %d. Current Mem=%ld\n", partTypeNow, PartTotalMem());
    tmpId = PartRegister(partTypeNow, 50, sizeof(PARTHEADER), 32);
    PrintValidPartTypes();

    partTypeNow = 3;
    printf("###### TEST unregistering first part type %d. Current Mem=%ld\n", partTypeNow, PartTotalMem());
    PartExpunge(3);
    PrintValidPartTypes();

    partTypeNow = 4;
    printf("###### TEST unregistering second part type %d. Current Mem=%ld\n", partTypeNow, PartTotalMem());
    PartExpunge(4);
    PrintValidPartTypes();

    printf("###### TEST allocating 3 parts from a particular part type. Current Mem=%ld\n", PartTotalMem());
    partTypeNow = 5;
    printf("\n\t{ Before registering mem type %d. Current Mem=%ld }\n", partTypeNow, PartTotalMem());
    PrintValidPartTypes();
    tmpId = PartRegister(partTypeNow, 100, sizeof(PARTBINLIST), 8);
    printf("\n\t{ After registering mem type %d. Current Mem=%ld }\n", partTypeNow, PartTotalMem());
    PrintValidPartTypes();
    tPtr1 = PartAlloc(partTypeNow);
    printf("\n\t{ Allocated part type %d = 0x%lx. Current Mem=%ld }\n", partTypeNow, tPtr1, PartTotalMem());
    tPtr2 = PartAlloc(partTypeNow);
    printf("\n\t{ Allocated part type %d = 0x%lx. Current Mem=%ld }\n", partTypeNow, tPtr2, PartTotalMem());
    tPtr3 = PartAlloc(partTypeNow);
    printf("\n\t{ Allocated part type %d = 0x%lx. Current Mem=%ld }\n", partTypeNow, tPtr3, PartTotalMem());
    PrintBinList(partBin[tmpId].head);

    printf("###### TEST deallocating 1 part from a particular part type. Current Mem=%ld\n", PartTotalMem());
	PartFree(partTypeNow, tPtr2);
    PrintBinList(partBin[tmpId].head);

    printf("###### Eliminating all parts. Current Mem=%ld\n", PartTotalMem());
	PartFree(partTypeNow, tPtr1);
	PartFree(partTypeNow, tPtr3);
    PartExpungeAll();
    printf("###### After eliminating all parts. Current Mem=%ld\n", PartTotalMem());

    return(0);
}

#endif    /* TESTMODULE */
