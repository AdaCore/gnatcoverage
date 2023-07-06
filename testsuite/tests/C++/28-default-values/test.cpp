namespace bar {
  bool b = true;
  bool foo (bool b = b) {
    return b;
  }
}

int main(){
  bar::foo();
  return 0;
}
