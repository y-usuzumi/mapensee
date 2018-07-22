package main

import (
	"flag"
	"fmt"
	"time"

	zmq "github.com/pebbe/zmq4"
	"github.com/sirupsen/logrus"
)

var (
	isSlave bool
	port    int
)

func parseArgs() {
	flag.BoolVar(&isSlave, "slave", false, "if true, run slave")
	flag.IntVar(&port, "port", 9988, "the port which the master listens to")
	flag.Parse()
}

func runMaster(port int) {
	socket, err := zmq.NewSocket(zmq.PULL)
	defer socket.Close()
	if err != nil {
		panic(fmt.Sprintf("Creating socket failed. %v", err))
	}
	socket.SetRcvhwm(1000)
	socket.Bind(fmt.Sprintf("tcp://*:%d", port))
	var i int
	for {
		msgs, err := socket.RecvMessage(0)
		if err != nil {
			logrus.Errorf("Error receiving message: %v", err)
			continue
		}
		for _, msg := range msgs {
			fmt.Printf("Got message %d: %s\n", i, msg)
		}
		time.Sleep(1000 * time.Microsecond)
		i++
	}
}

func runSlave(port int) {
	socket, err := zmq.NewSocket(zmq.PUSH)

	defer socket.Close()
	if err != nil {
		panic(fmt.Sprintf("Creating socket failed. %v", err))
	}
	socket.SetSndhwm(1000)
	socket.Connect(fmt.Sprintf("tcp://127.0.0.1:%d", port))
	for i := 0; i < 2000; i++ {
		fmt.Printf("%d: ", i)
		m := make([]byte, 10000)
		total, err := socket.SendMessageDontwait(string(m), string(m))
		if err != nil {
			logrus.Errorf("Error sending message: %v", err)
			continue
		}
		fmt.Printf("Sent %d bytes\n", total)
		time.Sleep(10 * time.Microsecond)
	}
	//time.Sleep(1 * time.Second)
}

func main() {
	parseArgs()
	fmt.Printf("isSlave: %t\n", isSlave)
	fmt.Printf("port: %d\n", port)
	if isSlave {
		runSlave(port)
	} else {
		runMaster(port)
	}
}
