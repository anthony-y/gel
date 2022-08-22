#pragma once

#include "arrays.h"
#include "common.h"
#include "result.h"

#include <assert.h>

static constexpr usize FNV_OFFSET_BASIS = 14695981039346656037;
static constexpr usize FNV_PRIME_VALUE = 1099511628211;
static constexpr usize TABLE_SEGMENT_SIZE = 64;

template<typename V>
struct Pair {
    Buffer key;
    V value;
};

template<typename V>
struct CollisionPair {
    Buffer key;
    V value;
    int slot;
};

template<typename V>
struct Table {
    Array<Pair<V>> backing;
    Array<u8> occupied;
    Table<V> *collisions;
};

template<typename V>
void table_init(Table<V> *table) {
    array_init(&table->backing, TABLE_SEGMENT_SIZE);
    array_init(&table->occupied, TABLE_SEGMENT_SIZE);
}

template<typename V>
void table_free(Table<V> *table) {
    array_free(&table->backing);
    array_free(&table->occupied);
}

static usize compute_string_hash(Buffer buffer) {
    usize hash = FNV_OFFSET_BASIS;

    for (int i = 0; i < buffer.length; i++) {
        char c = buffer.data[i];
        hash ^= c;
        hash *= FNV_PRIME_VALUE;
    }

    return hash;
}

template<typename V>
void table_append(Table<V> *table, Buffer key, V value) {
    int hash = compute_string_hash(key);
    if (table->occupied.length+1 >= TABLE_SEGMENT_SIZE) {
        assert(false);
    }
    assert(table->occupied.length == table->backing.length);
    int idx = hash % TABLE_SEGMENT_SIZE;
    if (table->occupied.data[idx] == 1) {
        // int collision_idx = table->collisions->length;
        assert(false);
    }
    Pair<V> pair = { .key = key, .value = value };
    table->backing.data[idx] = pair;
    table->occupied.data[idx] = 1;
}

template<typename V>
Result<V> table_get(Table<V> table, Buffer key) {
    int hash = compute_string_hash(key);
    assert(table.occupied.length == table.backing.length);
    int idx = hash % TABLE_SEGMENT_SIZE;
    if (table.occupied.data[idx] == 0) {
        return Err_Result(V);
    }
    V v = table.backing.data[idx].value;
    return Result<V> { .ok = v, .tag = Ok };
}

template<typename V>
void table_replace(Table<V> *table, Buffer key, V value) {
    int hash = compute_string_hash(key);
    assert(table->occupied.length == table->backing.length);
    int idx = hash % TABLE_SEGMENT_SIZE;
    if (table->occupied.data[idx] == 0) {
        return;
    }
    Pair<V> *pair = &table->backing.data[idx];
    pair->value = value;
}