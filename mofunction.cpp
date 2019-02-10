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
  TestCopyOnly() {}
  ~TestCopyOnly() {}
  TestCopyOnly(const TestCopyOnly& that) : copied{that.copied} {
    if (copied) {
      (*copied)++;
    }
  }

  TestCopyOnly& operator=(const TestCopyOnly& that) {
    copied = that.copied;
    return *this;
  }

  TestCopyOnly(size_t* ptr) : copied{ptr} {}

  void operator()() const& {}
  void operator()() & {}
  void operator()() && {}

  size_t* copied = nullptr;
};

struct TestDestroy {
  TestDestroy() {}
  ~TestDestroy() {
    if (destroyed) {
      (*destroyed)++;
    }
  }

  TestDestroy(size_t* ptr) : destroyed{ptr} {}

  void operator()() const& {}
  void operator()() & {}
  void operator()() && {}

  size_t* destroyed = nullptr;
};

struct TestCopy {
  TestCopy() {}
  ~TestCopy() {}

  TestCopy(const TestCopy&) {}
  TestCopy& operator=(const TestCopy&) { return *this; }

  TestCopy(TestCopy&&) {}
  TestCopy& operator=(TestCopy&&) { return *this; }

  void operator()() const& {}
  void operator()() & {}
  void operator()() && {}
};

struct TestMoveOnly {
  TestMoveOnly() {}
  ~TestMoveOnly() {}

  TestMoveOnly(const TestMoveOnly&) = delete;
  TestMoveOnly& operator=(const TestMoveOnly&) = delete;

  TestMoveOnly(TestMoveOnly&& that) : moved{that.moved} {
    if (moved) {
      (*moved)++;
    }
  }

  TestMoveOnly& operator=(TestMoveOnly&& that) {
    moved = that.moved;
    return *this;
  }

  TestMoveOnly(size_t* ptr) : moved{ptr} {}

  void operator()() const& {}
  void operator()() & {}
  void operator()() && {}

  size_t* moved = nullptr;
};

auto test_empty_lambda = [] {};
void test_empty_function() {}

void test_function(size_t* count) {
  if (count) {
    (*count)++;
  }
}

auto test_lambda = [](size_t* count) {
  if (count) {
    (*count)++;
  }
};

struct TestCallable {
  void operator()(size_t* count) {
    if (count) {
      (*count)++;
    }
  }
};

struct TestConstCallable {
  void operator()(size_t* count) const {
    if (count) {
      (*count)++;
    }
  }
};

struct TestRvrefCallable {
  void operator()(size_t* count) && {
    if (count) {
      (*count)++;
    }
  }
};

// ==== Unit Tests
// ============================================================

int main() {
  // ==== Targetting Constructors
  {
    std::cout << "default constructed nofunction is untargeted\n";
    std::mofunction<void()> f;
    assert(!f);
  }
  {
    std::cout << "nullptr constructed nofunction is untargeted\n";
    std::mofunction<void()> f(nullptr);
    assert(!f);
  }
  {
    std::cout << "nullptr constructed nofunction equivalent to nullptr\n";
    std::mofunction<void()> f(nullptr);
    assert(f == nullptr);
  }
  {
    std::cout << "function pointer constructed nofunction is targeted\n";
    std::mofunction<void()> f(test_empty_function);
    assert(f);
  }
  {
    std::cout << "function pointer constructed nofunction not equivalent to "
                 "nullptr\n";
    std::mofunction<void()> f(test_empty_function);
    assert(f != nullptr);
  }
  {
    std::cout << "lambda constructed nofunction is targeted\n";
    std::mofunction<void()> f(test_empty_lambda);
    assert(f);
  }
  {
    std::cout << "move-only object constructed nofunction is targeted\n";
    std::mofunction<void()> f(TestMoveOnly{});
    assert(f);
  }
  {
    std::cout << "move+copyable object constructed nofunction is targeted\n";
    std::mofunction<void()> f(TestCopy{});
    assert(f);
  }
  {
    std::cout << "destroyable object constructed nofunction is targeted\n";
    std::mofunction<void()> f(TestDestroy{});
    assert(f);
  }
  {
    std::cout << "copy-only object constructed nofunction is targeted\n";
    std::mofunction<void()> f(TestCopyOnly{});
    assert(f);
  }

  // ==== Copy/Move Constructors
  {
    std::cout << "move-only object constructed nofunction is moved\n";
    size_t moved = 0;
    std::mofunction<void()> f(TestMoveOnly{&moved});
    assert(moved > 0);
  }
  {
    std::cout << "destroyable object constructed nofunction is destroyed\n";
    size_t destroyed = 0;
    std::mofunction<void()> f(TestDestroy{&destroyed});
    assert(destroyed > 0);
  }
  {
    std::cout << "copy-only object constructed nofunction is copied\n";
    size_t copied = 0;
    std::mofunction<void()> f(TestCopyOnly{&copied});
    assert(copied > 0);
  }
  {
    std::cout << "move constructed nofunction preserves untargeted\n";
    std::mofunction<void()> f;
    std::mofunction<void()> g(std::move(f));
    assert(!g);
  }
  {
    std::cout << "move constructed nofunction preserves targeted\n";
    std::mofunction<void()> f(test_empty_function);
    std::mofunction<void()> g(std::move(f));
    assert(g);
  }

  // ==== Bad Call
  {
    std::cout << "untargetted nofunction throws bad_function_call\n";
    size_t caught = 0;
    try {
      std::mofunction<void()> f;
      f();
    } catch (const std::bad_function_call& e) {
      caught++;
    }
    assert(caught > 0);
  }

  // ==== Non-const signature, Non-const object, Lv-Callable
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: lambda "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: callable "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: rvref "
                 "callable not constructed\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Non-const signature, Const object, Lv-Callable
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: function not "
                 "called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(test_function);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: lambda not "
                 "called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(test_lambda);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: callable not "
                 "called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: const "
                 "callable not called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*)> f(TestConstCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: rvref "
                 "callable not called\n";
    size_t called = 0;
    // const std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Non-const signature, Non-const object, Rv-Callable
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: lambda "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: callable "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: rvref "
                 "callable not constructed\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Const signature, Non-const object, Lv-Callable
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout
        << "Const signature, Non-const object, Lv-Callable: lambda called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: callable not "
                 "constructed\n";
    size_t called = 0;
    // std::mofunction<void(size_t*) const> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: rvref "
                 "callable not constructed\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Const signature, Const object, Lv-Callable
  {
    std::cout
        << "Const signature, Const object, Lv-Callable: function called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) const> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: lambda called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: callable not "
                 "constructed\n";
    size_t called = 0;
    // const std::mofunction<void(size_t*) const> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: const callable "
                 "called\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) const> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: rvref callable "
                 "not constructed\n";
    size_t called = 0;
    // const std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Const signature, Non-const object, Rv-Callable
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout
        << "Const signature, Non-const object, Rv-Callable: lambda called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: callable not "
                 "constructed\n";
    size_t called = 0;
    // std::mofunction<void(size_t*) const> f(TestCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: rvref "
                 "callable nto constructed\n";
    size_t called = 0;
    // std::mofunction<void(size_t*)> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Rvref signature, Non-const object, Lv-Callable
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: function not "
                 "callabe\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_function);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: lambda not "
                 "callabe\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_lambda);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: callabe not "
                 "callabe\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: const "
                 "callabe not "
                 "callabe\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: rvref "
                 "callable not callabe\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Rvref signature, Const object, Lv-Callable
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: function not "
                 "callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(test_function);
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: lambda not "
                 "callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(test_lambda);
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: callable not "
                 "callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(TestCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: const callable "
                 "not callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) const> f(TestConstCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: rvref callable "
                 "not callable\n";
    size_t called = 0;
    const std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Rvref signature, Non-const object, Rv-Callable
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout
        << "Rvref signature, Non-const object, Rv-Callable: lambda called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: callable "
                 "called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: rvref "
                 "callable called\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }

  // ==== Target Copy/Move Constructors
  {
    std::cout
        << "callable object constructed nofunction is move constructible\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g(std::move(f));
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed nofunction is move assignable\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g;
    g = std::move(f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed nofunction is not copy "
                 "constructible\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    // std::mofunction<void(size_t*)> g(f);
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout
        << "callable object constructed nofunction is not copy assignable\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g;
    // f = g;
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "const callable object constructed nofunction is move "
                 "constructible\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::mofunction<void(size_t*) const> g(std::move(f));
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed nofunction is move "
                 "assignable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::mofunction<void(size_t*) const> g;
    g = std::move(f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed nofunction is move "
                 "constructible\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    std::mofunction<void(size_t*) &&> g(std::move(f));
    std::move(g)(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed nofunction is move "
                 "assignable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    std::mofunction<void(size_t*) &&> g;
    g = std::move(f);
    std::move(g)(&called);
    assert(called > 0);
  }

  // ==== Swap Operations
  {
    std::cout << "callable object constructed nofunction is swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g;
    f.swap(g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed nofunction is ADL swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*)> f(TestCallable{});
    std::mofunction<void(size_t*)> g;
    std::swap(f, g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed nofunction is swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::mofunction<void(size_t*) const> g;
    f.swap(g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout
        << "const callable object constructed nofunction is ADL swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) const> f(TestConstCallable{});
    std::mofunction<void(size_t*) const> g;
    std::swap(f, g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed nofunction is swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    std::mofunction<void(size_t*) &&> g;
    f.swap(g);
    assert(!f);
    std::move(g)(&called);
    assert(called > 0);
  }
  {
    std::cout
        << "rvref callable object constructed nofunction is ADL swappable\n";
    size_t called = 0;
    std::mofunction<void(size_t*) &&> f(TestRvrefCallable{});
    std::mofunction<void(size_t*) &&> g;
    std::swap(f, g);
    assert(!f);
    std::move(g)(&called);
    assert(called > 0);
  }

  return 0;
}
