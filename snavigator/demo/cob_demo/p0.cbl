       IDENTIFICATION DIVISION.
          AUTHOR. Josef Grosch
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         SELECT   VSVKPCA   ASSIGN      VSVKPCA
         RECORD KEY PCA-KEY      IN  VSVKPCA-REC1.
       DATA DIVISION.
       FILE SECTION.
         FD  VSVKPCA LABEL RECORD STANDARD.
         01 VSVKPCA-REC1.
            05 PCA-KEY.
       working-storage section.
         77 x.
         78 ccc value is 123.
         01 a.
           03 b.
      *
       PROCEDURE DIVISION.
         NOTE ein zweizeiliger
                          Kommentar . 
       p0.
         stop run.
       s1 section.
         p1.
           go to p2.
           entry e4711.
           go to p2 of s1.
         p2.
           move a to b.
           add b of a to pca-key of vsvkpca-rec1.
           go to s2.
         s2 section.
           go to p2.
           go to p2 of s1.
