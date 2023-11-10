/**
 At the moment the compiler does not raise any error,
 it just skips the deadcode
 */
int main()
{
  int i;
  if(i==2){
    return 0;
    i = 5;
  }
  i = 32; /* Error: code after a return */
}
