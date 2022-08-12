#pragma once

#include "common.h"

#include <stdlib.h>
#include <assert.h> // TODO: remove this
#include <string.h>

#if 0
template<typename T> struct Array<T> {
    T     *data;
    usize  length;
}
#endif

template<typename T> struct Array {
    T *data;
    int length;
    int capacity;

    T& operator[](u64 index) {
        return data[index];
    }
};

template<typename T> void array_init(Array<T> *a, u64 capacity) {
    a->capacity = capacity;
    T *data = (T *)calloc(capacity, sizeof(T)); 
    assert(data);
    a->data = data;
    a->length = 0;
}

template<typename T> int array_append(Array<T> *a, T &&item) {
    if (a->length >= a->capacity) {
        a->capacity += a->capacity;
        T* new_data = (T *)realloc(a->data, sizeof(T) * a->capacity);
        if (!new_data) return -1;
        assert(new_data);
        a->data = new_data;
    }
    a->data[a->length] = item;
    return a->length++;
    // memcpy(a->data + a->length++, item, sizeof(item));
}

template<typename T> int array_append(Array<T> *a, const T &item) {
    if (a->length >= a->capacity) {
        a->capacity += a->capacity;
        T* new_data = (T *)realloc(a->data, sizeof(T) * a->capacity);
        if (!new_data) return -1;
        assert(new_data);
        a->data = new_data;
    }
    a->data[a->length] = item;
    return a->length++;
}

template<typename T> void array_free(Array<T> *a) {
    free(a->data);
	a->length = 0;
	a->capacity = 0;
}

template<typename T> struct BucketArray {
    static constexpr int BUCKET_MAX_ITEMS = 64;

    struct Bucket {
        T items[BUCKET_MAX_ITEMS];
        BucketArray<T>::Bucket *next_bucket = nullptr;
        u64 length = 0;
    };

    BucketArray<T>::Bucket *first_bucket = nullptr;
    BucketArray<T>::Bucket *current_bucket = nullptr;
    u64 length = 1;

    T operator[](u64 index) {
        Bucket *next = first_bucket;
        int transient_index = 0;
        while (next) {
            Bucket *current = next;
            for (u64 i = 0; i < current->length; i++) {
                if (transient_index == index) {
                    return current->items[i];
                }
                transient_index++;
            }
            next = current->next_bucket;
        }
        assert(false);
    }
};

template<typename T> void bucket_array_init(BucketArray<T> *out) {
    auto memory = (typename BucketArray<T>::Bucket *)malloc(sizeof(typename BucketArray<T>::Bucket));
    assert(memory);
    *memory = {};
    out->first_bucket = memory;
    out->current_bucket = memory;
}

template<typename T> T *bucket_array_append(BucketArray<T> *to, const T &item) {
    if (to->current_bucket->length >= BucketArray<T>::BUCKET_MAX_ITEMS) {
        auto new_bucket = (typename BucketArray<T>::Bucket *)malloc(sizeof(typename BucketArray<T>::Bucket));
        assert(new_bucket);
        *new_bucket = {};
        to->current_bucket->next_bucket = new_bucket;
        to->current_bucket = new_bucket;
        to->length++;
    }
    to->current_bucket->items[to->current_bucket->length] = item;
    return &to->current_bucket->items[to->current_bucket->length++];
}

#if 0
template<typename T> void bucket_array_append(BucketArray<T> *to, T item) {
    if (to->current_bucket->length >= BucketArray<T>::BUCKET_MAX_ITEMS) {
        auto new_bucket = (typename BucketArray<T>::Bucket *)malloc(sizeof(typename BucketArray<T>::Bucket));
        assert(new_bucket);
        *new_bucket = {};
        to->current_bucket->next_bucket = new_bucket;
        to->current_bucket = new_bucket;
        to->length++;
    }
    to->current_bucket->items[to->current_bucket->length++] = item;
}
#endif

template<typename T> void bucket_array_free(BucketArray<T> *array) {
    typename BucketArray<T>::Bucket *next = array->first_bucket;
    while (next) {
        typename BucketArray<T>::Bucket *current = next;
        next = current->next_bucket;
        free(current);
    }
    *array = BucketArray<T>{0};
}

