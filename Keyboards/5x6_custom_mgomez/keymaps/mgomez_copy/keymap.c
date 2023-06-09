/* A standard layout for the Dactyl Manuform 5x6 Keyboard */

#include QMK_KEYBOARD_H

#define _QWERTY 0
#define _LOWER 1
#define _RAISE 2
#define Dvorak 3

#define RAISE MO(_RAISE)
#define LOWER MO(_LOWER)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

    [_QWERTY] = LAYOUT_5x6(
			   KC_ESC, KC_1, KC_2, KC_3, KC_4, KC_5,              KC_6, KC_7, KC_8, KC_9, KC_0, KC_BSPC,
			   KC_TAB, KC_Q, KC_W, KC_E, KC_R, KC_T,              KC_Y, KC_U, KC_I, KC_O, KC_P, KC_MINS,
			   KC_LSFT, KC_A, KC_S, KC_D, KC_F, KC_G,             KC_H, KC_J, KC_K, KC_L, KC_SCLN, KC_QUOT,
			   KC_LCTL, KC_Z, KC_X, KC_C, KC_V, KC_B,             KC_N, KC_M, KC_COMM, KC_DOT, KC_SLSH, KC_BSLS,
			               KC_LBRC, KC_RBRC,                                  KC_PLUS, KC_EQL,
			   KC_SPC, MO(_LOWER),                 KC_LEFT_GUI, LGUI(KC_ENTER),
			                              KC_KP_4, KC_BSPC,  KC_KP_ENTER, KC_KP_4,
			                             KC_LALT, KC_LCTL,  KC_RALT, KC_RCTL),

    [_LOWER] = LAYOUT_5x6(
			   KC_TILD, KC_EXLM, KC_AT, KC_HASH, KC_DLR, KC_PERC,          KC_CIRC, KC_AMPR, KC_ASTR, KC_LPRN, KC_RPRN, KC_DEL,
			   KC_TAB, KC_Q, SGUI(KC_NO), LCA(KC_NO), KC_R, KC_T,                 KC_Y, KC_U, KC_UP, KC_O, KC_P, KC_MINS,
			   KC_LSFT,KC_LALT, KC_S, KC_D, KC_TAB, KC_G,                 KC_H, KC_LEFT, KC_DOWN, KC_RIGHT, KC_SCLN, KC_QUOT,
			   KC_LCTL, KC_Z, KC_X, MEH(KC_E), KC_V, KC_B,                MO(2), KC_M, KC_COMM, KC_DOT, KC_SLSH, KC_BSLS,
			               KC_LBRC, KC_RBRC,                                    KC_PLUS, KC_EQL,
			                       KC_KP_5, KC_KP_6,              KC_KP_5, KC_KP_6,
			                              KC_KP_1, KC_KP_2,   KC_KP_1, KC_KP_2,
			                             KC_KP_3,  KC_KP_4,   KC_KP_3, KC_KP_4),

    [_RAISE] = LAYOUT_5x6(
			   KC_ESC,  KC_1,  KC_2,  KC_3,  KC_4,  KC_5,              KC_6, KC_7, KC_8, KC_9, KC_0, KC_BSPC,
			   KC_TAB, MEH(KC_E), KC_W, KC_E, LCTL(LSFT(KC_C)), KC_T,              KC_Y, KC_U, KC_I, KC_O, KC_P, KC_MINS,
			   KC_LSFT, KC_A, KC_S, KC_D, KC_F, KC_G,             KC_H, KC_J, KC_K, KC_L, KC_SCLN, KC_QUOT,
			   KC_LCTL, KC_Z, KC_X, KC_C, KC_V, KC_B,             KC_N, KC_M, KC_COMM, KC_DOT, KC_SLSH, KC_BSLS,
			               KC_LBRC, KC_RBRC,                                  KC_PLUS, KC_EQL,
			                       KC_KP_5, KC_KP_6,          KC_KP_5, KC_KP_6,
			                              KC_KP_1, KC_KP_2,  KC_KP_1, KC_KP_2,
			                             KC_KP_3,  KC_KP_4,  KC_KP_3, KC_KP_4),

};
