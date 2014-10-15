
char andeqor (char A, char B, char C, char D)
{
  if ((A && B) == (C || D)) // # eval
    return (char) 1; // # true
  else
    return (char) 0; // # false
}
