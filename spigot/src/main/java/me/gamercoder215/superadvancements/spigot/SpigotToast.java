package me.gamercoder215.superadvancements.spigot;

import me.gamercoder215.superadvancements.advancement.AFrame;
import me.gamercoder215.superadvancements.advancement.AToast;
import net.md_5.bungee.api.chat.BaseComponent;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

/**
 * Represents a Toast Notification for Spigot.
 */
public final class SpigotToast extends AToast {

    private BaseComponent message;

    /**
     * Creates a new Toast Notification.
     * @param icon ItemStack Icon
     * @param frame Toast Frame
     * @param message Toast Message
     * @throws IllegalArgumentException if message is null
     */
    public SpigotToast(@NotNull ItemStack icon, @NotNull AFrame frame, @NotNull BaseComponent message) throws IllegalArgumentException {
        super(icon, frame);

        if (message == null) throw new IllegalArgumentException("message cannot be null!");
        this.message = message;
    }

    /**
     * Creates a new Toast Notification.
     * @param m Material Icon
     * @param frame Toast Frame
     * @param message Toast Message
     * @throws IllegalArgumentException if message is null
     */
    public SpigotToast(@NotNull Material m, @NotNull AFrame frame, @NotNull BaseComponent message) throws IllegalArgumentException {
        this(new ItemStack(m), frame, message);
    }

    /**
     * Gets the message of the Toast Notification.
     * @return Toast Message
     */
    @NotNull
    public BaseComponent getMessage() {
        return message;
    }

    /**
     * Sets the message of the Toast Notification.
     * @param message Toast Message
     * @throws IllegalArgumentException if message is null
     */
    @NotNull
    public void setMessage(@NotNull BaseComponent message) throws IllegalArgumentException {
        if (message == null) throw new IllegalArgumentException("message cannot be null!");
        this.message = message;
    }

    @Override
    public int hashCode() {
        return Objects.hash(message.toPlainText());
    }

    @Override
    public void send(Player p) {

    }
}
