package me.gamercoder215.superadvancements.advancement;

import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

/**
 * Represents a Toast Notification
 */
public abstract class AToast {

    private ItemStack icon;
    private AFrame frame;

    /**
     * Creates a new Toast Notification.
     * @param icon ItemStack Icon
     * @param frame Toast Frame
     * @throws IllegalArgumentException If the icon or frame is null
     */
    protected AToast(@NotNull ItemStack icon, @NotNull AFrame frame) throws IllegalArgumentException {
        if (icon == null) throw new IllegalArgumentException("Icon cannot be null!");
        if (frame == null) throw new IllegalArgumentException("Frame cannot be null!");

        this.icon = icon;
        this.frame = frame;
    }

    /**
     * Gets the icon of the Toast Notification.
     * @return ItemStack Icon
     */
    @NotNull
    public ItemStack getIcon() {
        return icon;
    }

    /**
     * Gets the frame of the Toast Notification.
     * @return Toast Frame
     */
    @NotNull
    public AFrame getFrame() {
        return frame;
    }

    /**
     * Sets the icon of the Toast Notification.
     * @param icon ItemStack Icon
     * @throws IllegalArgumentException If the icon is null
     */
    public void setIcon(@NotNull ItemStack icon) throws IllegalArgumentException {
        if (icon == null) throw new IllegalArgumentException("Icon cannot be null!");
        this.icon = icon;
    }

    /**
     * Sets the frame of the Toast Notification.
     * @param frame Toast Frame
     * @throws IllegalArgumentException If the frame is null
     */
    public void setFrame(@NotNull AFrame frame) throws IllegalArgumentException {
        if (frame == null) throw new IllegalArgumentException("Frame cannot be null!");
        this.frame = frame;
    }

    public abstract int hashCode();

    /**
     * Sends the Toast to the player.
     * @param p Player to send to
     */
    public abstract void send(@NotNull Player p);
}
