package me.gamercoder215.superadvancements.advancement;

/**
 * The visibility of this Advancement in the menu.
 */
public enum AVisibility {

    /**
     * The Advancement is shown in the menu.
     */
    SHOWN,

    /**
     * The Advancement is shown in the menu if the parent Advancement has been unlocked.
     */
    PARENT_GRANTED,

    /**
     * The Advancement is shown when any ancestor or descendant (parent or children) have been granted.
     */
    DEFAULT,

    /**
     * The Advancement is hidden in the menu.
     */
    HIDDEN,

}
