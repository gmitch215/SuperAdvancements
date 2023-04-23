package me.gamercoder215.superadvancements.paper;

import net.kyori.adventure.text.Component;
import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.HoverEvent;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

public class TestPaperUtil {

    @Test
    @DisplayName("Test PaperUtil#toSpigot - Component")
    public void testToSpigot() {
        Component test = Component.text("Test");

        Assertions.assertNotNull(PaperUtil.toSpigot(test));
        Assertions.assertTrue(PaperUtil.toSpigot(test) instanceof net.md_5.bungee.api.chat.TextComponent);
    }

    @Test
    @DisplayName("Test PaperUtil#toSpigot - HoverEvent")
    public void testToSpigotHoverEvent() {
        net.kyori.adventure.text.event.HoverEvent<Component> test = net.kyori.adventure.text.event.HoverEvent.showText(Component.text("Test"));
        Assertions.assertNotNull(PaperUtil.toSpigot(test));

        Component test2 = Component.text("Test");
        test2 = test2.hoverEvent(test);

        BaseComponent translated = PaperUtil.toSpigot(test2);
        Assertions.assertNotNull(translated);
        Assertions.assertNotNull(translated.getHoverEvent());
        Assertions.assertEquals(translated.getHoverEvent().getAction(), net.md_5.bungee.api.chat.HoverEvent.Action.SHOW_TEXT);
    }

    @Test
    @DisplayName("Test PaperUtil#toSpigot - ClickEvent")
    public void testToSpigotClickEvent() {
        net.kyori.adventure.text.event.ClickEvent test = net.kyori.adventure.text.event.ClickEvent.runCommand("/test");
        Assertions.assertNotNull(PaperUtil.toSpigot(test));

        Component test2 = Component.text("Test");
        test2 = test2.clickEvent(test);

        BaseComponent translated = PaperUtil.toSpigot(test2);
        Assertions.assertNotNull(translated);
        Assertions.assertNotNull(translated.getClickEvent());
        Assertions.assertEquals(translated.getClickEvent().getValue(), "/test");
    }

    // To Paper

    @Test
    @DisplayName("Test PaperUtil#toPaper - BaseComponent")
    public void testToPaper() {
        net.md_5.bungee.api.chat.TextComponent test = new net.md_5.bungee.api.chat.TextComponent("Test");

        Assertions.assertNotNull(PaperUtil.toPaper(test));
        Assertions.assertTrue(PaperUtil.toPaper(test) instanceof net.kyori.adventure.text.TextComponent);
    }

    @Test
    @DisplayName("Test PaperUtil#toPaper - HoverEvent")
    public void testToPaperHoverEvent() {
        HoverEvent test = new HoverEvent(HoverEvent.Action.SHOW_TEXT, new BaseComponent[] {new net.md_5.bungee.api.chat.TextComponent("Test")});
        Assertions.assertNotNull(PaperUtil.toPaper(test));

        net.md_5.bungee.api.chat.TextComponent test2 = new net.md_5.bungee.api.chat.TextComponent("Test");
        test2.setHoverEvent(test);

        Component translated = PaperUtil.toPaper(test2);
        Assertions.assertNotNull(translated);
        Assertions.assertNotNull(translated.hoverEvent());
        Assertions.assertEquals(translated.hoverEvent().value(), Component.text("Test"));
    }

    @Test
    @DisplayName("Test PaperUtil#toPaper - ClickEvent")
    public void testToPaperClickEvent() {
        net.md_5.bungee.api.chat.ClickEvent test = new net.md_5.bungee.api.chat.ClickEvent(net.md_5.bungee.api.chat.ClickEvent.Action.RUN_COMMAND, "/test");
        Assertions.assertNotNull(PaperUtil.toPaper(test));

        net.md_5.bungee.api.chat.TextComponent test2 = new net.md_5.bungee.api.chat.TextComponent("Test");
        test2.setClickEvent(test);

        Component translated = PaperUtil.toPaper(test2);
        Assertions.assertNotNull(translated);
        Assertions.assertNotNull(translated.clickEvent());
        Assertions.assertEquals(translated.clickEvent().value(), "/test");
    }

}
