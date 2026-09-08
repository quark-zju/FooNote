#ifndef FNOTE_NOTEBACKEND_H
#define FNOTE_NOTEBACKEND_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void notebackend_stack_clear(void);
void notebackend_stack_pop(void);
void notebackend_stack_push_i32(int32_t value);
void notebackend_stack_push_str(const uint8_t *offset, size_t size);
int32_t notebackend_stack_last_i32(int32_t *ret);
int32_t notebackend_stack_last_str(const uint8_t **offset, size_t *size);

/* Safe for a zero-length string. Rust's from_raw_parts still requires a
 * non-null pointer when the length is zero. */
void notebackend_stack_push_str_safe(const char *value, size_t size);

int32_t notebackend_open_root_url(void);
void notebackend_close_all(void);
int32_t notebackend_get_children(void);
int32_t notebackend_get_text(void);
int32_t notebackend_insert(void);
int32_t notebackend_set_text(void);
int32_t notebackend_remove(void);
int32_t notebackend_persist(void);

#ifdef __cplusplus
}
#endif

#endif
