// Copyright 2019: Ryan McDougall, Matt Calabrese

#include <cassert>
#include <cstddef>
#include <functional>
#include <iostream>
#include <memory>
#include <type_traits>
#include <utility>

template <class Signature> struct any_invocable_base;
template <class Signature> struct any_invocable_interface;
template <class Signature, class Callable> struct any_invocable_implementation;

template <class MemFunPtr> struct member_function_ptr_to_signature;

template <bool IsNoexcept, class R, class F, class... Args>
constexpr bool is_nothrow_propagating_invocable_v = std::is_invocable_r_v<R, F, Args...>;

template <class R, class F, class... Args>
constexpr bool is_nothrow_propagating_invocable_v<true, R, F, Args...> =
    std::is_nothrow_invocable_r_v<R, F, Args...>;

#define ANY_INVOCABLE_IMPL_OF_(cvref_quals, inv_quals, noex)                                       \
  template <typename R, typename C, typename... P>                                                 \
  struct member_function_ptr_to_signature<R (C::*)(P...) cvref_quals noex> {                       \
    using type = R(P...) cvref_quals noex;                                                         \
  };                                                                                               \
                                                                                                   \
  template <typename R, typename... P> struct any_invocable_interface<R(P...) cvref_quals noex> {  \
    virtual R invoke(P...) noex = 0;                                                               \
    virtual ~any_invocable_interface() {}                                                          \
  };                                                                                               \
                                                                                                   \
  template <typename R, typename... P, typename Callable>                                          \
  struct any_invocable_implementation<R(P...) cvref_quals noex, Callable>                          \
      : any_invocable_interface<R(P...) cvref_quals noex> {                                        \
    template <typename F> explicit any_invocable_implementation(F &&f) : c(std::forward<F>(f)) {}  \
    virtual R invoke(P... args) noex override {                                                    \
      return std::invoke(static_cast<Callable inv_quals>(c), static_cast<P &&>(args)...);          \
    }                                                                                              \
                                                                                                   \
    [[no_unique_address]] Callable c;                                                              \
  };                                                                                               \
                                                                                                   \
  template <typename R, typename... P> struct any_invocable_base<R(P...) cvref_quals noex> {       \
    using result_type = R;                                                                         \
    template <typename F>                                                                          \
    using requirements =                                                                           \
        std::enable_if_t<is_nothrow_propagating_invocable_v<noex(false), R, F inv_quals, P...>,    \
                         bool>;                                                                    \
                                                                                                   \
    R operator()(P... args) cvref_quals noex { return f->invoke(static_cast<P &&>(args)...); }     \
                                                                                                   \
    std::unique_ptr<any_invocable_interface<R(P...) cvref_quals noex>> f;                          \
  }

#define ANY_INVOCABLE_IMPL_OF(cvref_quals, added_ref_of_qual_opt)                                  \
  ANY_INVOCABLE_IMPL_OF_(cvref_quals, cvref_quals added_ref_of_qual_opt, );                        \
  ANY_INVOCABLE_IMPL_OF_(cvref_quals, cvref_quals added_ref_of_qual_opt, noexcept)

ANY_INVOCABLE_IMPL_OF(, &);
ANY_INVOCABLE_IMPL_OF(const, &);

ANY_INVOCABLE_IMPL_OF(&, );
ANY_INVOCABLE_IMPL_OF(const &, );

ANY_INVOCABLE_IMPL_OF(&&, );
ANY_INVOCABLE_IMPL_OF(const &&, );

ANY_INVOCABLE_IMPL_OF(volatile, &);
ANY_INVOCABLE_IMPL_OF(volatile const, &);

ANY_INVOCABLE_IMPL_OF(volatile &, );
ANY_INVOCABLE_IMPL_OF(volatile const &, );

ANY_INVOCABLE_IMPL_OF(volatile &&, );
ANY_INVOCABLE_IMPL_OF(volatile const &&, );

#undef ANY_INVOCABLE_IMPL_OF
#undef ANY_INVOCABLE_IMPL_OF_

namespace std {
template <typename Signature> class any_invocable : any_invocable_base<Signature> {
private:
  template <typename F>
  using invoke_requirements = typename any_invocable_base<Signature>::template requirements<F>;

  template <typename F>
  using callable_construct_requirements =
      std::enable_if_t<!std::is_same_v<std::remove_cvref_t<F>, any_invocable> &&
                           !std::is_same_v<std::remove_cvref_t<F>, in_place_type_t<F>> &&
                           std::is_constructible_v<std::decay_t<F>, F> &&
                           invoke_requirements<F>(true),
                       bool>;

  template <typename F, typename... Args>
  using inplace_construct_requirements = std::enable_if_t<
      std::is_constructible_v<std::decay_t<F>, Args...> && invoke_requirements<F>(true), bool>;

  template <typename F, typename U, typename... Args>
  using inplace_initlist_construct_requirements =
      std::enable_if_t<std::is_constructible_v<std::decay_t<F>, initializer_list<U> &, Args...> &&
                           invoke_requirements<F>(true),
                       bool>;

public:
  using result_type = typename any_invocable_base<Signature>::result_type;
  using any_invocable_base<Signature>::operator();

  any_invocable() noexcept = default;
  any_invocable(std::nullptr_t) noexcept {}
  any_invocable &operator=(std::nullptr_t) noexcept {
    this->f = nullptr;
    return *this;
  }

  any_invocable(any_invocable &&) noexcept = default;
  any_invocable &operator=(any_invocable &&) = default;

  template <typename F, callable_construct_requirements<F> = true>
  any_invocable(F &&f)
      : any_invocable_base<Signature>{
            std::make_unique<any_invocable_implementation<Signature, F>>(std::forward<F>(f))} {}

  template <typename F, invoke_requirements<std::decay_t<F>> = true>
  any_invocable &operator=(F &&f) {
    any_invocable(std::forward<F>(f)).swap(*this);
    return *this;
  }

  template <typename F, typename... Args, inplace_construct_requirements<F, Args...> = true>
  any_invocable(in_place_type_t<F>, Args &&... args)
      : any_invocable_base<Signature>{std::make_unique<any_invocable_implementation<Signature, F>>(
            std::forward<Args>(args)...)} {}

  template <typename F, typename U, typename... Args,
            inplace_initlist_construct_requirements<F, U, Args...> = true>
  any_invocable(in_place_type_t<F>, initializer_list<U> init, Args &&... args)
      : any_invocable_base<Signature>{std::make_unique<any_invocable_implementation<Signature, F>>(
            init, std::forward<Args>(args)...)} {}

  explicit operator bool() const noexcept { return static_cast<bool>(this->f); }
  void swap(any_invocable &other) noexcept { this->f.swap(other.f); }

private:
  friend void swap(any_invocable &lhs, any_invocable &rhs) noexcept { lhs.swap(rhs); }

  friend bool operator==(const any_invocable &lhs, std::nullptr_t) noexcept {
    return !static_cast<bool>(lhs);
  }

  friend bool operator==(std::nullptr_t, const any_invocable &rhs) noexcept {
    return !static_cast<bool>(rhs);
  }

  friend bool operator!=(const any_invocable &lhs, std::nullptr_t) noexcept {
    return static_cast<bool>(lhs);
  }

  friend bool operator!=(std::nullptr_t, const any_invocable &rhs) noexcept {
    return static_cast<bool>(rhs);
  }
};

template <typename F>
any_invocable(F)
    -> any_invocable<typename member_function_ptr_to_signature<decltype(&F::operator())>::type>;

} // namespace std

// ==== Test Harness ==========================================================

struct TestNotCallable {};

struct TestCopyOnly {
  TestCopyOnly() {}
  ~TestCopyOnly() {}
  TestCopyOnly(const TestCopyOnly &that) : copied{that.copied} {
    if (copied) {
      (*copied)++;
    }
  }

  TestCopyOnly &operator=(const TestCopyOnly &that) {
    copied = that.copied;
    return *this;
  }

  TestCopyOnly(size_t *ptr) : copied{ptr} {}

  void operator()() const & {}
  void operator()() & {}
  void operator()() && {}

  size_t *copied = nullptr;
};

struct TestDestroy {
  TestDestroy() {}
  ~TestDestroy() {
    if (destroyed) {
      (*destroyed)++;
    }
  }

  TestDestroy(size_t *ptr) : destroyed{ptr} {}

  void operator()() const & {}
  void operator()() & {}
  void operator()() && {}

  size_t *destroyed = nullptr;
};

struct TestCopy {
  TestCopy() {}
  ~TestCopy() {}

  TestCopy(const TestCopy &) {}
  TestCopy &operator=(const TestCopy &) { return *this; }

  TestCopy(TestCopy &&) {}
  TestCopy &operator=(TestCopy &&) { return *this; }

  void operator()() const & {}
  void operator()() & {}
  void operator()() && {}
};

struct TestMoveOnly {
  TestMoveOnly() {}
  ~TestMoveOnly() {}

  TestMoveOnly(const TestMoveOnly &) = delete;
  TestMoveOnly &operator=(const TestMoveOnly &) = delete;

  TestMoveOnly(TestMoveOnly &&that) : moved{that.moved} {
    if (moved) {
      (*moved)++;
    }
  }

  TestMoveOnly &operator=(TestMoveOnly &&that) {
    moved = that.moved;
    return *this;
  }

  TestMoveOnly(size_t *ptr) : moved{ptr} {}

  void operator()() const & {}
  void operator()() & {}
  void operator()() && {}

  size_t *moved = nullptr;
};

auto test_empty_lambda = [] {};
void test_empty_function() {}

void test_function(size_t *count) {
  if (count) {
    (*count)++;
  }
}

auto test_lambda = [](size_t *count) {
  if (count) {
    (*count)++;
  }
};

struct TestCallable {
  void operator()(size_t *count) {
    if (count) {
      (*count)++;
    }
  }
};

struct TestConstCallable {
  void operator()(size_t *count) const {
    if (count) {
      (*count)++;
    }
  }
};

struct TestRvrefCallable {
  void operator()(size_t *count) && {
    if (count) {
      (*count)++;
    }
  }
};

// ==== Unit Tests ============================================================

int main() {
  // ==== Targetting Constructors
  {
    std::cout << "default constructed any_invocable is untargeted\n";
    std::any_invocable<void()> f;
    assert(!f);
  }
  {
    std::cout << "nullptr constructed any_invocable is untargeted\n";
    std::any_invocable<void()> f(nullptr);
    assert(!f);
  }
  {
    std::cout << "nullptr constructed any_invocable equivalent to nullptr\n";
    std::any_invocable<void()> f(nullptr);
    assert(f == nullptr);
  }
  {
    std::cout << "function pointer constructed any_invocable is targeted\n";
    std::any_invocable<void()> f(test_empty_function);
    assert(f);
  }
  {
    std::cout << "function pointer constructed any_invocable not equivalent to "
                 "nullptr\n";
    std::any_invocable<void()> f(test_empty_function);
    assert(f != nullptr);
  }
  {
    std::cout << "lambda constructed any_invocable is targeted\n";
    std::any_invocable<void()> f(test_empty_lambda);
    assert(f);
  }
  {
    std::cout << "move-only object constructed any_invocable is targeted\n";
    std::any_invocable<void()> f(TestMoveOnly{});
    assert(f);
  }
  {
    std::cout << "move+copyable object constructed any_invocable is targeted\n";
    std::any_invocable<void()> f(TestCopy{});
    assert(f);
  }
  {
    std::cout << "destroyable object constructed any_invocable is targeted\n";
    std::any_invocable<void()> f(TestDestroy{});
    assert(f);
  }
  {
    std::cout << "copy-only object constructed any_invocable is targeted\n";
    std::any_invocable<void()> f(TestCopyOnly{});
    assert(f);
  }

  // ==== Copy/Move Constructors
  {
    std::cout << "move-only object constructed any_invocable is moved\n";
    size_t moved = 0;
    std::any_invocable<void()> f(TestMoveOnly{&moved});
    assert(moved > 0);
  }
  {
    std::cout << "destroyable object constructed any_invocable is destroyed\n";
    size_t destroyed = 0;
    std::any_invocable<void()> f(TestDestroy{&destroyed});
    assert(destroyed > 0);
  }
  {
    std::cout << "copy-only object constructed any_invocable is copied\n";
    size_t copied = 0;
    std::any_invocable<void()> f(TestCopyOnly{&copied});
    assert(copied > 0);
  }
  {
    std::cout << "move constructed any_invocable preserves untargeted\n";
    std::any_invocable<void()> f;
    std::any_invocable<void()> g(std::move(f));
    assert(!g);
  }
  {
    std::cout << "move constructed any_invocable preserves targeted\n";
    std::any_invocable<void()> f(test_empty_function);
    std::any_invocable<void()> g(std::move(f));
    assert(g);
  }

  // ==== Non-const signature, Non-const object, Lv-Callable
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: lambda "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: callable "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Lv-Callable: rvref "
                 "callable not constructed\n";
    size_t called = 0;
    // std::any_invocable<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Non-const signature, Const object, Lv-Callable
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: function not "
                 "called\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *)> f(test_function);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: lambda not "
                 "called\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *)> f(test_lambda);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: callable not "
                 "called\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *)> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: const "
                 "callable not called\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *)> f(TestConstCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Non-const signature, Const object, Lv-Callable: rvref "
                 "callable not called\n";
    size_t called = 0;
    // const std::any_invocable<void(size_t*)> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Non-const signature, Non-const object, Rv-Callable
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: lambda "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: callable "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Non-const signature, Non-const object, Rv-Callable: rvref "
                 "callable not constructed\n";
    size_t called = 0;
    // std::any_invocable<void(size_t*)> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Const signature, Non-const object, Lv-Callable
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: lambda called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: callable not "
                 "constructed\n";
    size_t called = 0;
    // std::any_invocable<void(size_t*) const> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Lv-Callable: rvref "
                 "callable not constructed\n";
    size_t called = 0;
    // std::any_invocable<void(size_t*) const> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Const signature, Const object, Lv-Callable
  {
    std::cout << "Const signature, Const object, Lv-Callable: function called\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *) const> f(test_function);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: lambda called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(test_lambda);
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: callable not "
                 "constructed\n";
    size_t called = 0;
    // const std::any_invocable<void(size_t*) const> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: const callable "
                 "called\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    f(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Const object, Lv-Callable: rvref callable "
                 "not constructed\n";
    size_t called = 0;
    // const std::any_invocable<void(size_t*) const> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Const signature, Non-const object, Rv-Callable
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: lambda called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: callable not "
                 "constructed\n";
    size_t called = 0;
    // std::any_invocable<void(size_t*) const> f(TestCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Const signature, Non-const object, Rv-Callable: rvref "
                 "callable nto constructed\n";
    size_t called = 0;
    // std::any_invocable<void(size_t*) const> f(TestRvrefCallable{});
    // std::move(f)(&called);
    assert(called == 0);
  }

  // ==== Rvref signature, Non-const object, Lv-Callable
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: function not "
                 "callabe\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(test_function);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: lambda not "
                 "callabe\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(test_lambda);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: callabe not "
                 "callabe\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: const "
                 "callabe not "
                 "callabe\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Lv-Callable: rvref "
                 "callable not callabe\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Rvref signature, Const object, Lv-Callable
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: function not "
                 "callable\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *) &&> f(test_function);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: lambda not "
                 "callable\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *) &&> f(test_lambda);
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: callable not "
                 "callable\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *) &&> f(TestCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: const callable "
                 "not callable\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    // f(&called);
    assert(called == 0);
  }
  {
    std::cout << "Rvref signature, Const object, Lv-Callable: rvref callable "
                 "not callable\n";
    size_t called = 0;
    const std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    // f(&called);
    assert(called == 0);
  }

  // ==== Rvref signature, Non-const object, Rv-Callable
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: function "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(test_function);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: lambda called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(test_lambda);
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: callable "
                 "called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: const "
                 "callable called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }
  {
    std::cout << "Rvref signature, Non-const object, Rv-Callable: rvref "
                 "callable called\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::move(f)(&called);
    assert(called > 0);
  }

  // ==== Target Copy/Move Constructors
  {
    std::cout << "callable object constructed any_invocable is move constructible\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    std::any_invocable<void(size_t *)> g(std::move(f));
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed any_invocable is move assignable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    std::any_invocable<void(size_t *)> g;
    g = std::move(f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed any_invocable is not copy "
                 "constructible\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    // std::any_invocable<void(size_t*)> g(f);
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "callable object constructed any_invocable is not copy assignable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    std::any_invocable<void(size_t *)> g;
    // f = g;
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "const callable object constructed any_invocable is move "
                 "constructible\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *) const> g(std::move(f));
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed any_invocable is move "
                 "assignable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *) const> g;
    g = std::move(f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed any_invocable is move "
                 "constructible\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::any_invocable<void(size_t *) &&> g(std::move(f));
    std::move(g)(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed any_invocable is move "
                 "assignable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::any_invocable<void(size_t *) &&> g;
    g = std::move(f);
    std::move(g)(&called);
    assert(called > 0);
  }

  // ==== Target Copy/Move Constructors with Const/Non-const Signature
  {
    std::cout << "non-const signature any_invocable is move constructible from "
                 "const signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *)> g(std::move(f));
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "non-const signature any_invocable is move assignable from const "
                 "signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *)> g;
    g = std::move(f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "non-const signature any_invocable is not move constructible "
                 "from const signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestConstCallable{});
    // std::any_invocable<void(size_t*) const> g(std::move(f));
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "non-const signature any_invocable is not move assignable from const "
                 "signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestConstCallable{});
    std::any_invocable<void(size_t *) const> g;
    // g = std::move(f);
    // g(&called);
    assert(called == 0);
  }

  // ==== Target Copy/Move Constructors with Rvref/Non-const Signature
  {
    std::cout << "non-const signature any_invocable is not move constructible "
                 "from rvref signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    // std::any_invocable<void(size_t*)> g(std::move(f));
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "non-const signature any_invocable is not move assignable from "
                 "rvref signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::any_invocable<void(size_t *)> g;
    // g = std::move(f);
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "non-const signature any_invocable is not move constructible "
                 "from const signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    // std::any_invocable<void(size_t*) &&> g(std::move(f));
    // g(&called);
    assert(called == 0);
  }
  {
    std::cout << "non-const signature any_invocable is not move assignable from "
                 "const signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    std::any_invocable<void(size_t *) &&> g;
    // g = std::move(f);
    // g(&called);
    assert(called == 0);
  }

  // ==== Swap Operations
  {
    std::cout << "callable object constructed any_invocable is swappable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    std::any_invocable<void(size_t *)> g;
    f.swap(g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "callable object constructed any_invocable is ADL swappable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *)> f(TestCallable{});
    std::any_invocable<void(size_t *)> g;
    std::swap(f, g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed any_invocable is swappable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *) const> g;
    f.swap(g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "const callable object constructed any_invocable is ADL swappable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *) const> g;
    std::swap(f, g);
    assert(!f);
    g(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed any_invocable is swappable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::any_invocable<void(size_t *) &&> g;
    f.swap(g);
    assert(!f);
    std::move(g)(&called);
    assert(called > 0);
  }
  {
    std::cout << "rvref callable object constructed any_invocable is ADL swappable\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::any_invocable<void(size_t *) &&> g;
    std::swap(f, g);
    assert(!f);
    std::move(g)(&called);
    assert(called > 0);
  }

  // ==== Swap with Const/Non-const Signature
  {
    std::cout << "non-const signature any_invocable is not swappable from const "
                 "signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *)> g;
    // f.swap(g);
    // assert(!f);
    // std::move(g)(&called);
    assert(called == 0);
  }
  {
    std::cout << "non-const signature any_invocable is not ADL swappable from "
                 "const signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) const> f(TestConstCallable{});
    std::any_invocable<void(size_t *)> g;
    // std::swap(f, g);
    // assert(!f);
    // std::move(g)(&called);
    assert(called == 0);
  }

  // ==== Swap with Rvref/Non-const Signature
  {
    std::cout << "non-const signature any_invocable is not swappable from rvref "
                 "signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::any_invocable<void(size_t *)> g;
    // f.swap(g);
    // assert(!f);
    // std::move(g)(&called);
    assert(called == 0);
  }
  {
    std::cout << "non-const signature any_invocable is not ADL swappable from "
                 "rvref signature\n";
    size_t called = 0;
    std::any_invocable<void(size_t *) &&> f(TestRvrefCallable{});
    std::any_invocable<void(size_t *)> g;
    // std::swap(f, g);
    // assert(!f);
    // std::move(g)(&called);
    assert(called == 0);
  }

  return 0;
}
