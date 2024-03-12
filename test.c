long testing = 1234;

static long bruh = 1234;

void assert_eq_u64(long long x, long long y);

void another2() {
    bruh = 12;
    assert_eq_u64(1, 2);
}

void another() {
    another2();
}
void entry() {
    another();
}
