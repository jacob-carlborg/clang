// RUN: %clang_cc1 -fsyntax-only -verify -std=c++11 -ftemplate-depth 16 %s

// DR1330: an exception specification for a function template is only
// instantiated when it is needed.

template<typename T> void f1(T*) throw(T); // expected-error{{incomplete type 'Incomplete' is not allowed in exception specification}}
struct Incomplete; // expected-note{{forward}}

void test_f1(Incomplete *incomplete_p, int *int_p) {
  f1(int_p);
  f1(incomplete_p); // expected-note{{instantiation of exception spec}}
}

template<typename T> struct A {
  template<typename U> struct B {
    static void f() noexcept(A<U>().n);
  };

  constexpr A() : n(true) {}
  bool n;
};

static_assert(noexcept(A<int>::B<char>::f()), "");

template<unsigned N> struct S {
  static void recurse() noexcept(noexcept(S<N+1>::recurse())); // \
  // expected-error {{no member named 'recurse'}} \
  // expected-note 9{{instantiation of exception spec}}
};
decltype(S<0>::recurse()) *pVoid1 = 0; // ok, exception spec not needed
decltype(&S<0>::recurse) pFn = 0; // ok, exception spec not needed

template<> struct S<10> {};
void (*pFn2)() noexcept = &S<0>::recurse; // expected-note {{instantiation of exception spec}}


template<typename T> T go(T a) noexcept(noexcept(go(a))); // \
// expected-error 16{{call to function 'go' that is neither visible}} \
// expected-note 16{{'go' should be declared prior to the call site}} \
// expected-error {{recursive template instantiation exceeded maximum depth of 16}} \
// expected-error {{use of undeclared identifier 'go'}} \

void f() {
  int k = go(0); // \
  // expected-note {{in instantiation of exception specification for 'go<int>' requested here}}
}


namespace dr1330_example {
  template <class T> struct A {
    void f(...) throw (typename T::X); // expected-error {{'int'}}
    void f(int);
  };

  int main() {
    A<int>().f(42);
  }

  int test2() {
    struct S {
      template<typename T>
      static int f() noexcept(noexcept(A<T>().f("boo!"))) { return 0; } // \
      // expected-note {{instantiation of exception spec}}
      typedef decltype(f<S>()) X;
    };
    S().f<S>(); // ok
    S().f<int>(); // expected-note {{instantiation of exception spec}}
  }
}

namespace core_19754_example {
  template<typename T> T declval() noexcept;

  template<typename T, typename = decltype(T(declval<T&&>()))>
  struct is_movable { static const bool value = true; };

  template<typename T>
  struct wrap {
    T val;
    void irrelevant(wrap &p) noexcept(is_movable<T>::value);
  };

  template<typename T>
  struct base {
     base() {}
     base(const typename T::type1 &);
     base(const typename T::type2 &);
  };

  template<typename T>
  struct type1 {
     wrap<typename T::base> base;
  };

  template<typename T>
  struct type2 {
     wrap<typename T::base> base;
  };

  struct types {
     typedef base<types> base;
     typedef type1<types> type1;
     typedef type2<types> type2;
  };

  base<types> val = base<types>();
}
