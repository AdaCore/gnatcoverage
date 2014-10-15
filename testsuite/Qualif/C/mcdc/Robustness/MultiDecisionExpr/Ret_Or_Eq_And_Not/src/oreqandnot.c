
char oreqandnot (char A, char B, char C, char D)
{
  return ((A || B) == (C && !D)); // # eval
}
