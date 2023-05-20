package me.gamercoder215.superadvancements.advancement;

/**
 * Different kinds of behavior flags for MC Advancements.
 */
public enum AFlag {

    /**
     * The advancement will display a Toast Message upon completion. Added by default.
     */
    TOAST,

    /**
     * Whether to hide this advancement and all its children from the advancement screen until it is completed.
     */
    HIDDEN,

    /**
     * The advancement will broadcast a message upon completion. Added by default.
     */
    MESSAGE,

    /**
     * If the advancement has multiple criteria, the advancement will not show a progress bar indicating how many criteria it has and how many are completed.
     */
    MERGE_CRITERIA

}
