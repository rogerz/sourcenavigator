package com.sun.source.enumtest;

public interface EnumTest {

	public enum Kinder {
		MONDAY,
		TUESDAY,
		WEDNESDAY,
		THURSDAY,
		FRIDAY
	};

	Kinder getKind();
}
