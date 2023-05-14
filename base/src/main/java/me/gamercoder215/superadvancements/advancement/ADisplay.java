package me.gamercoder215.superadvancements.advancement;

import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Abstract Implementation of an Advancement's Display in the player's Advancement Menu.
 */
public abstract class ADisplay {

    private ItemStack icon;
    private AVisibility visibility;
    private String backgroundTexture;
    private float x, y = 0;
    private AFrame frame;

    /**
     * Creates a new Advancement Display.
     */
    protected ADisplay() {}

    /**
     * Creates a new Advancement Display.
     * @return New Advancement Display
     */
    @NotNull
    public ItemStack getIcon() {
        return icon;
    }

    /**
     * Sets the icon of the Advancement.
     * @param icon ItemStack Icon
     * @throws IllegalArgumentException If the icon is null
     */
    public void setIcon(@NotNull ItemStack icon) throws IllegalArgumentException {
        if (icon == null) throw new IllegalArgumentException("Icon cannot be null!");
        this.icon = icon;
    }

    /**
     * Gets the visibility of the Advancement.
     * @return Advancement Visibility
     */
    @NotNull
    public AVisibility getVisibility() {
        return visibility;
    }

    /**
     * Sets the visibility of the Advancement.
     * @param visibility Advancement Visibility
     * @throws IllegalArgumentException If the visibility is null
     */
    public void setVisibility(@NotNull AVisibility visibility) throws IllegalArgumentException {
        if (visibility == null) throw new IllegalArgumentException("Visibility cannot be null!");
        this.visibility = visibility;
    }

    /**
     * Gets the background texture of the Advancement.
     * @return Texture String
     * @see #setBackgroundTexture(String)
     */
    @Nullable
    public String getBackgroundTexture() {
        return backgroundTexture;
    }

    /**
     * <p>Sets the background texture of the Advancement.</p>
     * <p>This should link to a file in one of the player's resource packs (or the Vanilla textures), such as:</p>
     * <p>{@code setBackgroundTexture("textures/block/stone.png")}</p>
     * @param texture Texture String
     */
    public void setBackgroundTexture(@Nullable String texture) {
        this.backgroundTexture = texture;
    }

    /**
     * Gets the X position of the Advancement in the menu.
     * @return X Float Position
     */
    public float getX() {
        return x;
    }

    /**
     * Sets the X position of the Advancement in the menu.
     * @param x X Float Position
     */
    public void setX(float x) {
        this.x = x;
    }

    /**
     * Gets the Y position of the Advancement in the menu.
     * @return Y Float Position
     */
    public float getY() {
        return y;
    }

    /**
     * Sets the Y position of the Advancement in the menu.
     * @param y Y Float Position
     */
    public void setY(float y) {
        this.y = y;
    }

    /**
     * Gets the Frame type of the Advancement.
     * @return Frame Type
     */
    @NotNull
    public AFrame getFrame() {
        return frame;
    }

    /**
     * Sets the Frame type of the Advancement.
     * @param frame Frame Type
     * @throws IllegalArgumentException If the frame is null
     */
    public void setFrame(@NotNull AFrame frame) throws IllegalArgumentException {
        if (frame == null) throw new IllegalArgumentException("Frame cannot be null!");
        this.frame = frame;
    }

    /**
     * Converts this Advancement Display to a String.
     * <p>Note: This method should return a string in the format of: {@code "<title> - <description>"}</p>
     * @return String
     */
    public abstract String toString();
}
