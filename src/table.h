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
static void table_init_n(Table<V> *table, int max_n) {

    static int n = 0;

    array_init(&table->backing, TABLE_SEGMENT_SIZE);
    array_init(&table->occupied, TABLE_SEGMENT_SIZE);
    if (n++ < max_n) {
        table->collisions = (Table<V> *)malloc(sizeof(Table<V>));
        table_init(table->collisions);
    } else {
        table->collisions = nullptr;
    }
}

template<typename V>
void table_init(Table<V> *table) {
    table_init_n(table, 4);
}

template<typename V>
void table_free(Table<V> *table) {
    array_free(&table->backing);
    array_free(&table->occupied);
    if (table->collisions) table_free(table->collisions);
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
        assert(false); // unreachable
    }

    assert(table->occupied.length == table->backing.length);

    int idx = hash % TABLE_SEGMENT_SIZE;

    if (table->occupied.data[idx] == 1) {
        auto existing = table->backing.data[idx];

        if (strncmp(existing.key.data, key.data, existing.key.length) == 0) {
            // It's a duplicate, you should have checked for this before (even though it's slow so yeah maybe this should return a Result)
            assert(false);
        }
        
        // It's not a duplicate, add it to the collisions table.
        if (table->collisions) {
            table_append(table->collisions, key, value);
        }

        return;
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

    Pair<V> v = table.backing.data[idx];

    // Maybe the key is different - in which case we may be returning a collision
    if (v.key.length != key.length || strncmp(v.key.data, key.data, v.key.length) != 0) {
        
        // Iterate all lower collision tables to find a matching pair.
        // This is slow but happens rarely.
        Table<V> *next = table.collisions;
        while (next) {

            if (next->occupied[idx]) {
                auto pair = next->backing[idx];

                if (pair.key.length == key.length && strncmp(pair.key.data, key.data, pair.key.length) == 0) {
                    return Result<V> { .ok = pair.value, .tag = Ok };
                }
            }

            next = next->collisions;
        }
    }

    return Result<V> { .ok = v.value, .tag = Ok };
}

template<typename V>
void table_replace(Table<V> *table, Buffer key, V value) {
    int hash = compute_string_hash(key);

    assert(table->occupied.length == table->backing.length);

    int idx = hash % TABLE_SEGMENT_SIZE;

    if (table->occupied.data[idx] == 0) {
        return;
    }

    Pair<V> *v = &table->backing.data[idx];

    if (v->key.length != key.length || strncmp(v->key.data, key.data, v->key.length) != 0) {

        Table<V> *next = table->collisions;
        while (next) {

            if (next->occupied[idx]) {
                Pair<V> *pair = &next->backing[idx];

                if (pair->key.length == key.length && strncmp(pair->key.data, key.data, pair->key.length) == 0) {
                    pair->value = value;
                    return;
                }
            }

            next = next->collisions;
        }
    }
    
    v->value = value;
}