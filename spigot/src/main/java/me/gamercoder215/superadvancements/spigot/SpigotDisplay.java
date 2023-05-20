package me.gamercoder215.superadvancements.spigot;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import net.md_5.bungee.api.chat.BaseComponent;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Represents an Advancement Display for Spigot.
 */
public final class SpigotDisplay extends ADisplay {

    private BaseComponent title;
    private BaseComponent description;

    /**
     * Constructs a new Spigot Advancement Display.
     * @param frame Frame Type
     */
    public SpigotDisplay(@NotNull AFrame frame) {
        super();
        setFrame(frame);
    }

    /**
     * Gets the title of the Advancement.
     * @return Title Component
     */
    @NotNull
    public BaseComponent getTitle() {
        return title;
    }

    /**
     * Sets the title of the Advancement.
     * @param title Title Component
     * @throws IllegalArgumentException If title is null
     */
    public void setTitle(@NotNull BaseComponent title) throws IllegalArgumentException {
        if (title == null) throw new IllegalArgumentException("Title cannot be null!");
        this.title = title;
    }

    /**
     * Gets the description of the Advancement.
     * @return Description Component, or null if not found
     */
    @Nullable
    public BaseComponent getDescription() {
        return description;
    }

    /**
     * Sets the description of the Advancement.
     * @param description Description Component
     */
    public void setDescription(@Nullable BaseComponent description) {
        this.description = description;
    }

    @Override
    public String getTitleAsString() {
        return getTitle().toPlainText();
    }

    @Override
    public String getDescriptionAsString() {
        return getDescription() == null ? "" : getDescription().toPlainText();
    }

    /**
     * Creates a new Builder for a Spigot Advancement Display.
     * @param frame Frame Type
     * @return Builder
     * @throws IllegalArgumentException If the frame is null
     */
    public static Builder builder(@NotNull AFrame frame) throws IllegalArgumentException {
        return new Builder(frame);
    }

    /**
     * Builder for a Spigot Advancement Display.
     */
    public static final class Builder {

        AFrame frame;
        BaseComponent title;
        BaseComponent description;
        float x, y;
        ItemStack icon;
        String backgroundTexture;

        private Builder(AFrame frame) {
            if (frame == null) throw new IllegalArgumentException("Frame cannot be null!");
            this.frame = frame;
        }

        /**
         * Sets the title of the Advancement.
         * @param title Title
         * @return this class, for chaining
         * @throws IllegalArgumentException If title is null
         */
        @NotNull
        public Builder title(@NotNull BaseComponent title) throws IllegalArgumentException {
            if (title == null) throw new IllegalArgumentException("Title cannot be null!");
            this.title = title;
            return this;
        }

        /**
         * Sets the description of the Advancement.
         * @param description Description
         * @return this class, for chaining
         */
        @NotNull
        public Builder description(@Nullable BaseComponent description) {
            this.description = description;
            return this;
        }

        /**
         * Sets the icon of the Advancement.
         * @param icon ItemStack Icon
         * @return this class, for chaining
         * @throws IllegalArgumentException If the icon is null
         */
        @NotNull
        public Builder icon(@NotNull ItemStack icon) throws IllegalArgumentException {
            if (icon == null) throw new IllegalArgumentException("Icon cannot be null!");
            this.icon = icon;
            return this;
        }

        /**
         * Sets the icon of the Advancement.
         * @param m Material Icon
         * @return this class, for chaining
         * @throws IllegalArgumentException If the icon is null
         */
        @NotNull
        public Builder icon(@NotNull Material m) throws IllegalArgumentException {
            if (m == null) throw new IllegalArgumentException("Icon cannot be null!");
            this.icon = new ItemStack(m);
            return this;
        }

        /**
         * Sets the background texture of the Advancement.
         * @param texture Texture String
         * @return this class, for chaining
         * @see #setBackgroundTexture(String)
         */
        @NotNull
        public Builder backgroundTexture(@Nullable String texture) {
            this.backgroundTexture = texture;
            return this;
        }

        /**
         * Sets the background texture of the Advancement.
         * @param m Material to use as the background texture
         * @return this class, for chaining
         * @see #setBackgroundTexture(Material)
         */
        @NotNull
        public Builder backgroundTexture(@Nullable Material m) {
            if (m == null)
                this.backgroundTexture = null;
            else
                this.backgroundTexture = "textures/" + (m.isBlock() ? "block" : "item") + "/" + m.name().toLowerCase() + ".png";

            return this;
        }

        /**
         * Sets the X and Y coordinates of the Advancement.
         * @param x X Coordinate
         * @param y Y Coordinate
         * @return this class, for chaining
         */
        @NotNull
        public Builder coordinates(float x, float y) {
            this.x = x;
            this.y = y;
            return this;
        }

        /**
         * Builds the Advancement Display.
         * @return New Advancement Display
         * @throws IllegalArgumentException If the title, frame, or icon is null
         */
        @NotNull
        public SpigotDisplay build() throws IllegalArgumentException {
            if (title == null) throw new IllegalArgumentException("Title cannot be null!");
            if (frame == null) throw new IllegalArgumentException("Frame cannot be null!");
            if (icon == null) throw new IllegalArgumentException("Icon cannot be null!");

            SpigotDisplay display = new SpigotDisplay(frame);

            display.setTitle(title);
            display.setDescription(description);
            display.setIcon(icon);
            display.setBackgroundTexture(backgroundTexture);
            display.setX(x);
            display.setY(y);

            return display;
        }

    }
}
