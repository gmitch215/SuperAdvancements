package me.gamercoder215.superadvancements.advancement;

import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Abstract Implementation of an Advancement's Display in the player's Advancement Menu.
 */
public abstract class ADisplay {

    private ItemStack icon;
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
     * <p>{@code setBackgroundTexture("textures/blocks/stone.png")}</p>
     * @param texture Texture String
     */
    public void setBackgroundTexture(@Nullable String texture) {
        this.backgroundTexture = texture;
    }

    /**
     * <p>Sets the background texture of the Advancement.</p>
     * <p>This sets the background texture to {@code textures/blocks/<material>.png} if {@link Material#isBlock()} returns true or {@code textures/items/<material>.png} if it returns false.</p>
     * @param m Material to use
     */
    public void setBackgroundTexture(@Nullable Material m) {
        if (m == null)
            this.backgroundTexture = null;
        else
            this.backgroundTexture = "textures/" + (m.isBlock() ? "blocks" : "items") + "/" + m.name().toLowerCase() + ".png";
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
     * Converts this Advancement Display's Title to a String.
     * @return Title String
     */
    @NotNull
    public abstract String getTitleAsString();

    /**
     * Converts this Advancement Display's Description to a String.
     * @return Description String, or blank if no description
     */
    @NotNull
    public abstract String getDescriptionAsString();
}
