package me.gamercoder215.superadvancements.advancement;

/**
 * Different kinds of behavior flags for MC Advancements.
 */
public enum AFlag {

    /**
     * The advancement will display a Toast Message upon completion.
     */
    TOAST,

    /**
     * The advancement will display a hidden "true" value, allowing the creation of empty Advancement Tabs or drawing lines.
     */
    HIDDEN_TRUE,

    /**
     * The advancement will broadcast a message upon completion.
     */
    MESSAGE,

    /**
     * The advancement only needs one of the criteria to be completed.
     */
    ONLY_ONE_CRITERIA

}
