// Copyright 2023 QMK
// SPDX-License-Identifier: GPL-2.0-or-later

#include QMK_KEYBOARD_H
#include "macpadmg.h"
#define MATRIX_COLS 5
#define MATRIX_ROWS 5

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    /*
     * ┌───┬───┬───┬───┬───┬───┐
     * │Esc│ ( │ ) │ / │ * │ - │
     * ├───┼───┼───┼───┼───┼───┤
     * │ A │ B │ 7 │ 8 │ 9 │   │
     * ├───┼───┼───┼───┼───┤ + │
     * │ C │ D │ 4 │ 5 │ 6 │   │
     * ├───┼───┼───┼───┼───┼───┤
     * │ E │ F │ 1 │ 2 │ 3 │   │
     * ├───┼───┼───┴───┼───┤Ent│
     * │Ctl│TG1│ 0     │ . │   │
     * └───┴───┴───────┴───┴───┘
     */
  [0] = NUMPAD_5x5(
	 KC_1,  KC_1,  KC_1,  KC_1,  KC_1,  
	 KC_2,  KC_2,  KC_2,  KC_2,  KC_2,  
	 KC_3,  KC_3,  KC_3,  KC_3,  KC_3,  
	 KC_4,  KC_4,_______,  KC_4, KC_4,  
	 KC_5,_______,_______,_______,KC_5  
	 
    )

};
