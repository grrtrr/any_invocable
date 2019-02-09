// Copyright 2019: Matt Calabrese, Ryan McDougall
//

#include <cassert>
#include <cstddef>
#include <functional>
#include <iostream>
#include <memory>
#include <type_traits>

namespace {
template <class Signature>
struct mofunction_interface;

template <class Result, class... Args>
struct mofunction_interface<Result(Args...)> {
  virtual Result invoke(Args...) = 0;
  virtual ~mofunction_interface() {}
};

template <class Result, class... Args>
struct mofunction_interface<Result(Args...) const> {
  virtual Result invoke(Args...) = 0;
  virtual ~mofunction_interface() {}
};

template <class Result, class... Args>
struct mofunction_interface<Result(Args...) &&> {
  virtual Result invoke(Args...) = 0;
  virtual ~mofunction_interface() {}
};

template <class Signature, class Callable>
struct mofunction_child;

template <class Result, class... Args, class Callable>
struct mofunction_child<Result(Args...), Callable>
    : mofunction_interface<Result(Args...)> {
  explicit mofunction_child(Callable&& c) : c(std::move(c)) {}
  virtual Result invoke(Args... args) override {
    if constexpr (std::is_void_v<Result>)
      std::invoke(static_cast<Callable&>(c), static_cast<Args&&>(args)...);
    else
      return std::invoke(static_cast<Callable&>(c),
                         static_cast<Args&&>(args)...);
  }

  [[no_unique_address]] Callable c;
};

template <class Result, class... Args, class Callable>
struct mofunction_child<Result(Args...) const, Callable>
    : mofunction_interface<Result(Args...) const> {
  explicit mofunction_child(Callable&& c) : c(std::move(c)) {}
  virtual Result invoke(Args... args) override {
    if constexpr (std::is_void_v<Result>)
      std::invoke(static_cast<const Callable&>(c),
                  static_cast<Args&&>(args)...);
    else
      return std::invoke(static_cast<const Callable&>(c),
                         static_cast<Args&&>(args)...);
  }

  [[no_unique_address]] Callable c;
};

template <class Result, class... Args, class Callable>
struct mofunction_child<Result(Args...)&&, Callable>
    : mofunction_interface<Result(Args...) &&> {
  explicit mofunction_child(Callable&& c) : c(std::move(c)) {}
  virtual Result invoke(Args... args) override {
    if constexpr (std::is_void_v<Result>)
      std::invoke(static_cast<Callable&&>(c), static_cast<Args&&>(args)...);
    else
      return std::invoke(static_cast<Callable&&>(c),
                         static_cast<Args&&>(args)...);
  }

  [[no_unique_address]] Callable c;
};

template <class Signature>
struct mofunction_impl;

template <class Result, class... Args>
struct mofunction_impl<Result(Args...)> {
  using result_type = Result;
  template <class F>
  using requirements =
      std::enable_if_t<std::is_invocable_r_v<Result, F, Args...>>*;

  Result operator()(Args... args) {
    return f ? f->invoke(static_cast<Args&&>(args)...)
             : throw std::bad_function_call();
  }

  std::unique_ptr<mofunction_interface<Result(Args...)>> f;
};

template <class Result, class... Args>
struct mofunction_impl<Result(Args...) const> {
  using result_type = Result;
  template <class F>
  using requirements =
      std::enable_if_t<std::is_invocable_r_v<Result, const F&, Args...>>*;

  Result operator()(Args... args) const {
    return f ? f->invoke(static_cast<Args&&>(args)...)
             : throw std::bad_function_call();
  }

  std::unique_ptr<mofunction_interface<Result(Args...) const>> f;
};

template <class Result, class... Args>
struct mofunction_impl<Result(Args...) &&> {
  using result_type = Result;
  template <class F>
  using requirements =
      std::enable_if_t<std::is_invocable_r_v<Result, F&&, Args...>>*;

  Result operator()(Args... args) && {
    return f ? f->invoke(static_cast<Args&&>(args)...)
             : throw std::bad_function_call();
  }

  std::unique_ptr<mofunction_interface<Result(Args...) &&>> f;
};
}  // namespace

namespace std {
template <class Signature>
class mofunction : mofunction_impl<Signature> {
 private:
  using base_t = mofunction_impl<Signature>;
  template <class F>
  using requirements = typename base_t::template requirements<F>;

 public:
  using result_type = typename base_t::result_type;
  using base_t::operator();

  mofunction() = default;
  mofunction(std::nullptr_t) noexcept {}

  template <class F, requirements<F> = nullptr>
  mofunction(F f)
      : base_t{std::make_unique<mofunction_child<Signature, F>>(std::move(f))} {
  }

  mofunction& operator=(std::nullptr_t) noexcept {
    this->f = nullptr;
    return *this;
  }

  template <class F, requirements<std::decay_t<F>> = nullptr>
  mofunction& operator=(F&& f) {
    return *this = mofunction(std::forward<F>(f));
  }

  template <class F, requirements<std::reference_wrapper<F>> = nullptr>
  mofunction& operator=(std::reference_wrapper<F> f) noexcept {
    return *this = mofunction(f);
  }

  void swap(mofunction& other) noexcept { this->f.swap(other.f); }
  explicit operator bool() const noexcept { return static_cast<bool>(this->f); }
};

template <class Signature>
bool operator==(const mofunction<Signature>& lhs, std::nullptr_t) noexcept {
  return !static_cast<bool>(lhs);
}

template <class Signature>
bool operator==(std::nullptr_t, const mofunction<Signature>& rhs) noexcept {
  return !static_cast<bool>(rhs);
}

template <class Signature>
bool operator!=(const mofunction<Signature>& lhs, std::nullptr_t) noexcept {
  return static_cast<bool>(lhs);
}

template <class Signature>
bool operator!=(std::nullptr_t, const mofunction<Signature>& rhs) noexcept {
  return static_cast<bool>(rhs);
}
}  // namespace std

// ==== Test Harness ==========================================================

struct TestNotCallable {};

struct TestCopyOnly {
  TestCopyOnly() { std::cout << "TestCopyOnly default construct\n"; }
  ~TestCopyOnly() { std::cout << "TestCopyOnly deconstruct\n"; }

  TestCopyOnly(const TestCopyOnly& that) : copied{that.copied} {
    std::cout << "TestCopyOnly copy construct\n";
    if (copied) {
      (*copied)++;
    }
  }

  TestCopyOnly& operator=(const TestCopyOnly& that) {
    std::cout << "TestCopyOnly copy assign\n";
    copied = that.copied;
    return *this;
  }

  TestCopyOnly(size_t* ptr) : copied{ptr} {
    std::cout << "TestCopyOnly argument construct\n";
  }

  void operator()() const& { std::cout << "TestCopyOnly const call\n"; }
  void operator()() & { std::cout << "TestCopyOnly lref call\n"; }
  void operator()() && { std::cout << "TestCopyOnly rref call\n"; }

  size_t* copied = nullptr;
};

struct TestDestroy {
  TestDestroy() { std::cout << "TestDestroy default construct\n"; }
  ~TestDestroy() {
    std::cout << "TestDestroy destruct\n";
    if (destroyed) {
      (*destroyed)++;
    }
  }

  TestDestroy(size_t* ptr) : destroyed{ptr} {
    std::cout << "TestDestroy argument construct\n";
  }

  void operator()() const& { std::cout << "TestDestroy const call\n"; }
  void operator()() & { std::cout << "TestDestroy lref call\n"; }
  void operator()() && { std::cout << "TestDestroy rref call\n"; }

  size_t* destroyed = nullptr;
};

struct TestCopy {
  TestCopy() { std::cout << "TestCopy default construct\n"; }
  ~TestCopy() { std::cout << "TestCopy deconstruct\n"; }

  TestCopy(const TestCopy&) { std::cout << "TestCopy copy construct\n"; }
  TestCopy& operator=(const TestCopy&) {
    std::cout << "TestCopy copy assign\n";
    return *this;
  }

  TestCopy(TestCopy&&) { std::cout << "TestCopy move construct\n"; }
  TestCopy& operator=(TestCopy&&) {
    std::cout << "TestCopy move assign\n";
    return *this;
  }

  void operator()() const& { std::cout << "TestCopy const call\n"; }
  void operator()() & { std::cout << "TestCopy lref call\n"; }
  void operator()() && { std::cout << "TestCopy rref call\n"; }
};

struct TestMoveOnly {
  TestMoveOnly() { std::cout << "TestMoveOnly default construct\n"; }
  ~TestMoveOnly() { std::cout << "TestMoveOnly destruct\n"; }

  TestMoveOnly(const TestMoveOnly&) = delete;
  TestMoveOnly& operator=(const TestMoveOnly&) = delete;

  TestMoveOnly(TestMoveOnly&& that) : moved{that.moved} {
    std::cout << "TestMoveOnly move construct\n";
    if (moved) {
      (*moved)++;
    }
  }

  TestMoveOnly& operator=(TestMoveOnly&& that) {
    std::cout << "TestMoveOnly move assign\n";
    moved = that.moved;
    return *this;
  }

  TestMoveOnly(size_t* ptr) : moved{ptr} {
    std::cout << "TestMoveOnly argument construct\n";
  }

  void operator()() const& { std::cout << "TestMoveOnly const call\n"; }
  void operator()() & { std::cout << "TestMoveOnly lref call\n"; }
  void operator()() && { std::cout << "TestMoveOnly rref call\n"; }

  size_t* moved = nullptr;
};

auto test_empty_lambda = [] {};
void test_empty_function() {}

void test_function(size_t* count) {
  std::cout << "test_function called\n";
  if (count) {
    (*count)++;
  }
}

auto test_lambda = [](size_t* count) {
  std::cout << "test_lambda called\n";
  if (count) {
    (*count)++;
  }
};

struct TestCallable {
  void operator()(size_t* count) {
    std::cout << "TestCallable call\n";
    if (count) {
      (*count)++;
    }
  }
};

struct TestConstCallable {
  void operator()(size_t* count) const {
    std::cout << "TestConstCallable call\n";
    if (count) {
      (*count)++;
    }
  }
};

struct TestRvrefCallable {
  void operator()(size_t* count) && {
    std::cout << "TestRvrefCallable call\n";
    if (count) {
      (*count)++;
    }
  }
};

// ==== Unit Tests ============================================================

int main() {
  std::cout << "Hello mofo\n";

  // ==== Targetting Constructors
  {
    std::cout << "default constructed mofo is untargeted\n";
    std::mofunction<void()> f;
    assert(!f);
  }
  {
    std::cout << "nullptr constructed mofo is untargeted\n";
    std::mofunction<void()> f(nullptr);
    assert(!f);
  }
  {
    std::cout << "nullptr constructed mofo equivalent to nullptr\n";
    std::mofunction<void()> f(nullptr);
    assert(f == nullptr);
  }
  {
    std::cout << "function pointer constructed mofo is targeted\n";
    std::mofunction<void()> f(test_empty_function);
    assert(f);
  }
  {
    std::cout
        << "function pointer constructed mofo not equivalent to nullptr\n";
    std::mofunction<void()> f(test_empty_function);
    assert(f != nullptr);
  }
  {
    std::cout << "lambda constructed mofo is targeted\n";
    std::mofunction<void()> f(test_empty_lambda);
    assert(f);
  }
  {
    std::cout << "move-only object constructed mofo is targeted\n";
    std::mofunction<void()> f(TestMoveOnly{});
    assert(f);
  }
  {
    std::cout << "move+copyable object constructed mofo is targeted\n";
    std::mofunction<void()> f(TestCopy{});
    assert(f);
  }
  {
    std::cout << "destroyable object constructed mofo is targeted\n";
    std::mofunction<void()> f(TestDestroy{});
    assert(f);
  }
  {
    std::cout << "copy-only object constructed mofo is targeted\n";
    std::mofunction<void()> f(TestCopyOnly{});
    assert(f);
  }

  // ==== Base Copy/Move Operations
  {
    std::cout << "move-only object constructed mofo is moved\n";
    size_t moved = 0;
    std::mofunction<void()> f(TestMoveOnly{&moved});
    assert(moved > 0);
  }
  {
    std::cout << "destroyable object constructed mofo is destroyed\n";
    size_t destroyed = 0;
    std::mofunction<void()> f(TestDestroy{&destroyed});
    assert(destroyed > 0);
  }
  {
    std::cout << "copy-only object constructed mofo is copied\n";
    size_t copied = 0;
    std::mofunction<void()> f(TestCopyOnly{&copied});
    assert(copied > 0);
  }
  {
    std::cout << "move constructed mofo preserves untargeted\n";
    std::mofunction<void()> f;
    std::mofunction<void()> g(std::move(f));
    assert(!g);
  }
  {
    std::cout << "move constructed mofo preserves targeted\n";
    std::mofunction<void()> f(test_empty_function);
    std::mofunction<void()> g(std::move(f));
    assert(g);
  }

  // ==== Bad Call Operations
  {
    std::cout << "untargetted mofo throws bad_function_call\n";
    size_t caught = 0;
    try {
      std::mofunction<void()> f;
      f();
    } catch (const std::bad_function_call& e) {
      caught++;
    }
    assert(caught > 0);
  }

  // ==== Base Call Operations
  {
    std::cout << "function pointer constructed mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "lambda constructed mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed mofo is not callable or "
                 "constructable\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Const Base Call Operations
  {
    std::cout << "function pointer constructed const mofo is not callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(test_function);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "lambda constructed const mofo is not callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(test_lambda);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "callable object constructed const mofo is not callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout
        << "const callable object constructed const mofo is not callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(TestConstCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "rvref callable object constructed const mofo is not callable "
                 "or constructable\n";
    size_t called = 0;
    // const std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Rvref Base Call Operations
  {
    std::cout << "function pointer constructed rvref mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "lambda constructed rvref mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed rvref mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed rvref mofo is called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed rvref mofo is not callable "
                 "or constructable\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Const Call Operations
  {
    std::cout << "function pointer constructed mofo is const called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "lambda constructed mofo is const called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is not const callable or "
                 "constructable\n";
    size_t called = 0;
    // std::mofunction<void(size_t*) const> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "const callable object constructed mofo is const called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed mofo is not const callable "
                 "or constructable\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Const Const Call Operations
  {
    std::cout << "function pointer constructed mofo is const called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) const> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "lambda constructed mofo is const called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is not const callable or "
                 "constructable\n";
    size_t called = 0;
    // const std::mofunction<void(size_t*) const> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "const callable object constructed mofo is const called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) const> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed mofo is not const callable "
                 "or constructable\n";
    size_t called = 0;
    // const std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Rvref Const Call Operations
  {
    std::cout << "function pointer constructed mofo is const called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "lambda constructed mofo is const called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is not const callable or "
                 "constructable\n";
    size_t called = 0;
    // std::mofunction<void(size_t*) const> f(TestCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "const callable object constructed mofo is const called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed mofo is not const callable "
                 "or constructable\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Rvref Call Operations
  {
    std::cout << "function pointer constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_function);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "lambda constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_lambda);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "callable object constructed mofo is rvref callable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "const callable object constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "rvref callable object constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Rvref Rvref Call Operations
  {
    std::cout << "function pointer constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "lambda constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is rvref callable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed mofo is rvref called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }

  // ==== Const Rvref Call Operations
  {
    std::cout
        << "function pointer constructed const mofo is not rvref callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(test_function);
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "lambda constructed const mofo is not rvref callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(test_lambda);
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "callable object constructed const mofo is rvref callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(TestCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "const callable object constructed const mofo is not rvref "
                 "callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) const> f(TestConstCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "rvref callable object constructed const mofo is not rvref "
                 "callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Target Copy/Move Operations
  {
    std::cout << "callable object constructed mofo is move constructible\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g(std::move(f));
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is move assignable\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g;
    g = std::move(f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is not copy constructible\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestCallable{});
    // std::mofunction<void(size_t*)> g(f);
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "callable object constructed mofo is not copy assignable\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestCallable{});
    // std::mofunction<void(size_t*)> g;
    // f = g;
    // g(&called);
    assert(called == 0);
  }

  // ==== Swap Operations
  {
    std::cout << "callable object constructed mofo is swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g;
    f.swap(g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed mofo is ADL swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g;
    std::swap(f, g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }

  std::cout << "Goodbye!\n";
  return 0;
}
