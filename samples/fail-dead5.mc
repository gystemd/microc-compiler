/**
 At the moment the compiler does not raise any error,
 it just skips the deadcode
 */
int foo(){
  int i = 0;
  if (i==0){
    return 0;
    i = 5;
  }
  int j = 0;
  return j;
}
int main()
{
  print(foo());
}
