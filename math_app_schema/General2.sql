-- phpMyAdmin SQL Dump
-- version 4.8.5
-- https://www.phpmyadmin.net/
--
-- Host: localhost
-- Generation Time: Jan 06, 2021 at 10:02 PM
-- Server version: 5.7.31-0ubuntu0.18.04.1
-- PHP Version: 7.2.24-0ubuntu0.18.04.6

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT = 0;
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `General2`
--

-- --------------------------------------------------------

--
-- Table structure for table `EMPTY`
--

CREATE TABLE `EMPTY` (
  `CODE1` text,
  `TEXT` text,
  `IMAGE` varchar(800) NOT NULL,
  `ID` bigint(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `EMPTY`
--

INSERT INTO `EMPTY` (`CODE1`, `TEXT`, `IMAGE`, `ID`) VALUES
('', '', '', 1),
('', '', '', 2),
('', '', '', 3),
('', '', '', 4),
('', '', '', 5),
('', '', '', 6),
('', '', '', 7),
('', '', '', 8),
('', '', '', 9),
('', '', '', 10);

-- --------------------------------------------------------

--
-- Table structure for table `Junction1`
--

CREATE TABLE `Junction1` (
  `ID` int(10) UNSIGNED NOT NULL,
  `USER_ID` int(11) UNSIGNED DEFAULT NULL,
  `PRESENTATION_TABLE_NAME` varchar(800) DEFAULT NULL,
  `TITLE` varchar(500) NOT NULL,
  `NSLIDES` int(10) UNSIGNED NOT NULL,
  `RPlot` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `presentation120200811000008`
--

CREATE TABLE `presentation120200811000008` (
  `CODE1` text,
  `LINES` text,
  `IMAGE` text,
  `ID` bigint(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `Users`
--

CREATE TABLE `Users` (
  `ID` int(11) UNSIGNED NOT NULL,
  `Email` varchar(320) NOT NULL,
  `Password` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `Junction1`
--
ALTER TABLE `Junction1`
  ADD PRIMARY KEY (`ID`),
  ADD KEY `userid` (`USER_ID`);

--
-- Indexes for table `Users`
--
ALTER TABLE `Users`
  ADD UNIQUE KEY `ID` (`ID`),
  ADD UNIQUE KEY `Email` (`Email`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `Junction1`
--
ALTER TABLE `Junction1`
  MODIFY `ID` int(10) UNSIGNED NOT NULL AUTO_INCREMENT;

--
-- AUTO_INCREMENT for table `Users`
--
ALTER TABLE `Users`
  MODIFY `ID` int(11) UNSIGNED NOT NULL AUTO_INCREMENT;
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
