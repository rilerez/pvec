#pragma once

#include <array>
#include <variant>
#include <memory>
#include <string>

struct assertion : public std::runtime_error {
  assertion(std::string x) : runtime_error(x) {}
};

#define assert(...)                                                            \
  if (!(__VA_ARGS__)) {                                                        \
    throw assertion(std::to_string(__LINE__));                                 \
  }

inline constexpr auto exp2(auto n) { return 1 << n; }

inline constexpr auto log2(unsigned int n) {
  return sizeof(n) * 8 - __builtin_clz(n) - 1;
}
inline constexpr auto log2(unsigned long n) {
  return sizeof(n) * 8 - __builtin_clzl(n) - 1;
}
inline constexpr auto log2(unsigned long long n) {
  return sizeof(n) * 8 - __builtin_clzll(n) - 1;
}

inline constexpr auto leq_multiple(auto exponent, auto x) {
  return (x >> exponent) << exponent;
}

inline constexpr auto is_pow_of_pow_of_2(auto exponent, auto n) {
  auto const lg2 = log2(n);
  return lg2 == leq_multiple(exponent, lg2);
}

inline constexpr auto geq_multiple(auto exponent, auto x) {
  return (x != 0) * (leq_multiple(exponent, x - 1) + exp2(exponent));
}

inline constexpr bool is_multiple(auto exponent, auto n) {
  return n == leq_multiple(exponent, n);
}

inline constexpr auto tail_start(unsigned int exponent, unsigned int size) {
  return (size != 0) * leq_multiple(exponent, size - 1);
}

inline constexpr auto which_child(unsigned int exponent,
                                  unsigned int remaining_depth,
                                  unsigned int key) {
  assert(exponent > 0);
  assert(remaining_depth > 0);
  unsigned int mask = exp2(exponent) - 1;
  return (key >> ((remaining_depth - 1) * exponent)) & mask;
}

template<size_t exponent>
struct with_exponent {
  // Free functions are easiest to test, but passing the exponent constantly
  // is error-prone.
#define WRAP_WITH_EXP(name)                                                    \
  template<class... Args>                                                      \
  static inline auto constexpr name(Args... args) {                            \
    return ::name(exponent, std::forward<Args>(args)...);                      \
  }
  WRAP_WITH_EXP(leq_multiple)
  WRAP_WITH_EXP(is_pow_of_pow_of_2)
  WRAP_WITH_EXP(geq_multiple)
  WRAP_WITH_EXP(is_multiple)
  WRAP_WITH_EXP(tail_start)
  WRAP_WITH_EXP(which_child)
#undef WRAP_WITH_EXP

  static constexpr auto node_size = exp2(exponent);
#if true
#  if true
  template<class T, template<class> class ptr, class size_type>
  struct with_node_data {
    struct pvec_node;

    struct pvec_node {
      using MiddleChild = ptr<pvec_node>;
      using Middles = std::array<MiddleChild, node_size>;
      using Leaves = std::array<T, node_size>;
      std::variant<Middles, Leaves> children;

      template<class InPlaceTag, class... Args>
      pvec_node(InPlaceTag tag, Args&&... args)
          : children{tag, std::forward<Args>(args)...} {}

#    define MAYBECONST(const_)                                                 \
      Middles const_& middles() const_ {                                       \
        using std::get;                                                        \
        return get<Middles>(children);                                         \
      }                                                                        \
      Leaves const_& leaves() const_ {                                         \
        using std::get;                                                        \
        return get<Leaves>(children);                                          \
      }                                                                        \
      MiddleChild const_& lookup_middle(size_type const key,                   \
                                        size_type const remaining_depth)       \
          const_ {                                                             \
        assert(remaining_depth > 1);                                           \
        return middles()[which_child(remaining_depth, key)];                   \
      }                                                                        \
      T const_& lookup_leaf(size_type const key) const_ {                      \
        return leaves()[which_child(1, key)];                                  \
      }
      MAYBECONST()
      MAYBECONST(const)
#    undef MAYBECONST
    };

#  endif
#endif

    template<template<class> class outer_ptr, class pvec_node = pvec_node>
    class pvec {
      using node_t = pvec_node;
      size_type size_;
      std::array<T, node_size> tail;
      outer_ptr<pvec_node> data = nullptr;
      size_type tail_start() const { return with_exponent::tail_start(size_); }
      size_type depth() const {
        return (tail_start() != 0) * log2(tail_start()) / exponent;
      }
      bool is_tail_full() const { return (!empty()) && is_multiple(size_); }
      bool is_data_full() const {
        return (!empty()) && is_pow_of_pow_of_2(tail_start());
      }

      pvec_node* lookup_penultimate(size_type const index) const {
        pvec_node* node = data.get();
        auto remaining_depth = depth();
        for (; remaining_depth > 1; --remaining_depth)
          node = node->lookup_middle(index, remaining_depth).get();
        return node;
      }

     public:
      auto size() const { return size_; }
      bool empty() const { return size() == 0; }

      T const& operator[](size_type const index) const {
        assert(size() > index);
        assert(index >= 0);
        auto const tail_start = this->tail_start();
        if (index >= tail_start)
          return tail[index - tail_start];
        else {
          return lookup_penultimate(index)->lookup_leaf(index);
        }
      }

     private:
      template<class NodeMaker, class... Args>
      void conj_when_full(NodeMaker node_maker, Args&&... args) {
        if (data == nullptr)
          data = node_maker.root_penultimate(std::move(tail));
        else if (is_data_full()) {
          // using scoping to avoid use-after-move
          auto const make_new_root = [&] {
            outer_ptr<pvec_node> new_root = node_maker.root();
            new_root->middles()[0] = node_maker.as_inner(data);
            auto const make_chain = [&]() {
              auto current = node_maker.penultimate(std::move(tail));
              auto const previous_depth = depth();
              for (int i = 0; i < previous_depth; ++i) {
                auto next = node_maker.middle();
                next->middles()[0] = current;
                current = next;
              }
              return current;
            };
            new_root->middles()[1] = make_chain();
            return new_root;
          };
          data = make_new_root();
        } else {
          data = node_maker.root(data);
          auto current_node = node_maker.as_inner(data);
          for (auto remaining_depth = depth(); remaining_depth > 2;
               --remaining_depth) {
            auto& next = current_node->lookup_middle(size(), remaining_depth);
            next = node_maker.as_inner(next);
            current_node = next;
          }
          current_node->lookup_middle(size(), 1) =
              node_maker.penultimate(std::move(tail));
        }
        tail[0] = T{std::forward<Args>(args)...};
      }

     public:
      template<class NodeMaker, class... Args>
      friend pvec conj(NodeMaker node_maker, pvec vec, Args&&... args) {
        if (vec.is_tail_full())
          [[unlikely]] vec.conj_when_full(node_maker,
                                          std::forward<Args>(args)...);
        else
          vec.tail[vec.size_ - vec.tail_start()] =
              T{std::forward<Args>(args)...};
        ++vec.size_;
        return vec;
      }

     private:
      template<class NodeMaker>
      void popped_when_full(NodeMaker node_maker) {
        assert(data != nullptr);
        tail = *lookup_penultimate(size());
        if (is_data_full())
          data = node_maker.root(data->middles()[0]);
      }

     public:
      template<class NodeMaker>
      friend pvec popped(NodeMaker node_maker, pvec vec) {
        assert(vec.size_ > 0);
        --vec.size_;
        if (vec.is_tail_full())
          vec.popped_when_full(node_maker);
        assert(vec.size_ >= 0);
        return vec;
      }
    };
  };
};

template<class pvec_node_>
struct shared_ptr_node_maker {
  using pvec_node = pvec_node_;
  using ptr = std::shared_ptr<pvec_node>;
  using outer_ptr = std::shared_ptr<pvec_node>;

  ptr as_inner(outer_ptr p) { return p; }

  outer_ptr root(outer_ptr p) { return std::make_shared<pvec_node>(*p); }

  ptr middle(pvec_node const& node) {
    return std::make_shared<pvec_node>(node);
  }
  template<class... Args>
  ptr middle(Args&&... args) {
    return std::make_shared<pvec_node>(
        std::in_place_type_t<typename pvec_node::Middles>{},
        std::forward<Args>(args)...);
  }

  template<class... Args>
  outer_ptr root(Args&&... args) {
    return middle(std::forward<Args>(args)...);
  }

  template<class... Args>
  ptr penultimate(Args&&... args) {
    return std::make_shared<pvec_node>(
        std::in_place_type_t<typename pvec_node::Leaves>{},
        std::forward<Args>(args)...);
  }

  template<class... Args>
  outer_ptr root_penultimate(Args&&... args) {
    return penultimate(std::forward<Args>(args)...);
  }
};
