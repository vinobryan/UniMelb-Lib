/* =============================================================== */

/* This function was adapted in 2012 from 
 * http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/pdb.c
 * (no longer available at that URL in 2013)
 * and was written in the first instance by Paul Bourke
 *
 * Modified for use in a different assignment previously by Alistair Moffat by:
 *   . changing the argument type to two structs type line_t
 *   . making sure result is TRUE if an endpoint is on the other line
 * Modified for use by Jianzhong Qi by:
 *   . testing whether the projections of the two line segments intersect first
 */
#define TRUE 1
#define FALSE 0
#define EPS (1e-06)
#define ABS(x) (fabs(x))
#define MIN(a,b) (a<b ? a:b)
#define MAX(a,b) (a>b ? a:b)

int line_intersect(line_t l1, line_t l2) {
   double x1=l1.p1.x, y1=l1.p1.y,
   	  x2=l1.p2.x, y2=l1.p2.y,
   	  x3=l2.p1.x, y3=l2.p1.y,
   	  x4=l2.p2.x, y4=l2.p2.y;
   double mua,mub;
   double denom,numera,numerb;

   /* Take the projections of the two line segments */
   double xMin1, xMax1, xMin2, xMax2, yMin1, yMax1, yMin2, yMax2;
   xMin1 = MIN(x1, x2);
   xMax1 = MAX(x1, x2);
   xMin2 = MIN(x3, x4);
   xMax2 = MAX(x3, x4);

   yMin1 = MIN(y1, y2);
   yMax1 = MAX(y1, y2);
   yMin2 = MIN(y3, y4);
   yMax2 = MAX(y3, y4);
   
   /* Do the projects intersect? */
   if((xMin2-xMax1) >= EPS || (xMin1-xMax2) >= EPS || 
   	   (yMin2-yMax1) >= EPS || (yMin1-yMax2) >= EPS) {
   	   return FALSE;
   }

   denom  = (y4-y3) * (x2-x1) - (x4-x3) * (y2-y1);
   numera = (x4-x3) * (y1-y3) - (y4-y3) * (x1-x3);
   numerb = (x2-x1) * (y1-y3) - (y2-y1) * (x1-x3);

   /* Are the line coincident? */
   if (ABS(numera) < EPS && ABS(numerb) < EPS && ABS(denom) < EPS) {
      return(TRUE);
   }

   /* Are the line parallel */
   if (ABS(denom) < EPS) {
      return(FALSE);
   }

   /* Is the intersection along the the segments */
   mua = numera / denom;
   mub = numerb / denom;
   /* AM - use equality here so that "on the end" is not an
    * intersection; use strict inequality if "touching at end" is an
    * intersection */
   if (mua < 0 || mua > 1 || mub < 0 || mub > 1) {
      return(FALSE);
   }
   return(TRUE);
}
